extern crate parity_wasm;
#[macro_use]
extern crate structopt;
extern crate unicode_xid;
extern crate unidecode;

use parity_wasm::deserialize_file;
use parity_wasm::elements::{External, FunctionType, ImportCountType, Internal, NameSection,
                            Opcode, Section, Type, ValueType};
use std::collections::BTreeMap;
use std::fs::File;
use std::io::{BufWriter, Write};
use std::path::PathBuf;
use structopt::StructOpt;
use unicode_xid::UnicodeXID;
use unidecode::unidecode_char;

mod code_builder;
mod expr_builder;
mod precedence;
mod reorder_analysis;

#[derive(StructOpt)]
struct Opt {
    #[structopt(
        short = "n",
        long = "use-name-section",
        help = "Use the names in the name section for the internal function names"
    )]
    use_name_section: bool,
    #[structopt(
        short = "c",
        long = "public-call-indirect",
        help = "Make indirect calling available in the API"
    )]
    public_call_indirect: bool,
    #[structopt(help = "Input file", parse(from_os_str))]
    input: PathBuf,
    #[structopt(
        help = "Output file, stored next to wasm file if not specified", parse(from_os_str)
    )]
    output: Option<PathBuf>,
}

pub struct Function<'a> {
    name: String,
    ty: &'a FunctionType,
    ty_index: u32,
    real_name: Option<&'a String>,
    make_public: bool,
}

pub struct Global<'a> {
    is_mutable: bool,
    is_pub: bool,
    name: String,
    ty: &'static str,
    value: String,
    init_code: Option<&'a [Opcode]>,
}

#[derive(Debug)]
pub enum BlockKind {
    Function {
        evaluates_to_value: bool,
    },
    Block {
        label: usize,
        dst_var: Option<String>,
    },
    If {
        label: usize,
        dst_var: Option<String>,
        is_breakable: bool,
    },
    Loop {
        label: usize,
        dst_var: Option<String>,
    },
}

fn to_rs_type(t: ValueType) -> &'static str {
    match t {
        ValueType::I32 => "i32",
        ValueType::I64 => "i64",
        ValueType::F32 => "f32",
        ValueType::F64 => "f64",
    }
}

use std::fmt;

struct Indentation(usize);

impl fmt::Display for Indentation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for _ in 0..self.0 {
            write!(f, "    ")?
        }
        Ok(())
    }
}

fn mangle_fn_name(name: &str) -> String {
    let mut s = String::new();
    for (i, mut c) in name.chars().enumerate() {
        if i == 0 {
            if UnicodeXID::is_xid_start(c) {
                s.push(c);
                continue;
            }
            s.push('_');
        }

        if UnicodeXID::is_xid_continue(c) {
            s.push(c);
            continue;
        }

        let decoded = unidecode_char(c);
        if decoded == "[?]" {
            if s.chars().last() != Some('_') {
                s.push('_');
            }
            continue;
        }

        for c in decoded.chars() {
            if UnicodeXID::is_xid_continue(c) {
                s.push(c);
            } else {
                if s.chars().last() != Some('_') {
                    s.push('_');
                }
            }
        }
    }
    s
}

fn call_indirect_name(f: &FunctionType) -> String {
    let mut s = String::from("call_indirect_");
    for param in f.params() {
        s.push_str(match *param {
            ValueType::I32 => "i",
            ValueType::I64 => "l",
            ValueType::F32 => "f",
            ValueType::F64 => "d",
        });
    }
    s.push_str(match f.return_type() {
        Some(ValueType::I32) => "_i",
        Some(ValueType::I64) => "_l",
        Some(ValueType::F32) => "_f",
        Some(ValueType::F64) => "_d",
        None => "_v",
    });
    s
}

fn main() {
    let opt = Opt::from_args();
    let input = opt.input;
    let output = opt.output.unwrap_or_else(|| input.with_extension("rs"));

    let module = deserialize_file(input).unwrap();
    let module = module.parse_names().unwrap_or_else(|(_, m)| m);

    let mut writer = BufWriter::new(File::create(output).expect("Couldn't create output file"));

    let import_count = module.import_count(ImportCountType::Function);
    let code = module.code_section().unwrap();
    let fns = module.function_section().unwrap();
    let types = module.type_section().unwrap();
    let exports = module.export_section().unwrap();
    let function_names = module
        .sections()
        .iter()
        .filter_map(|s| match *s {
            Section::Name(NameSection::Function(ref s)) => Some(s),
            _ => None,
        })
        .next();

    let mut functions = Vec::new();

    if let Some(imports) = module.import_section() {
        for import in imports.entries() {
            // TODO Handle modules
            if let &External::Function(ty_index) = import.external() {
                let typ: &Type = &types.types()[ty_index as usize];
                let fn_type = match *typ {
                    Type::Function(ref t) => t,
                };
                functions.push(Function {
                    name: import.field().to_owned(),
                    ty: fn_type,
                    ty_index,
                    real_name: None,
                    make_public: false,
                });
            }
        }
    }

    for function in fns.entries() {
        let ty_index = function.type_ref();
        let Type::Function(ref fn_type) = types.types()[ty_index as usize];
        let mut real_name = function_names.and_then(|f| f.names().get(functions.len() as _));
        let mut name = format!("func{}", functions.len());
        if opt.use_name_section {
            if let Some(real_name) = real_name.take() {
                name = real_name.to_string();
                while functions.iter().any(|f| f.name == name) {
                    name.push_str("_");
                }
            }
        }
        functions.push(Function {
            name,
            ty: fn_type,
            ty_index,
            real_name,
            make_public: false,
        });
    }

    for function in &mut functions {
        function.name = mangle_fn_name(&function.name);
    }

    writeln!(
        writer,
        "#![allow(
    unreachable_code, dead_code, unused_assignments, unused_mut, unused_variables, non_snake_case,
    non_upper_case_globals, unused_parens, unconditional_recursion
)]

pub const PAGE_SIZE: usize = 64 << 10;

pub trait Imports {{
    type Memory: Memory;"
    ).unwrap();

    for function in &functions[..import_count] {
        write!(
            writer,
            "    fn {}(&mut self, context: &mut Context<Self::Memory>",
            function.name
        ).unwrap();
        for (i, &param) in function.ty.params().iter().enumerate() {
            write!(writer, ", var{}: {}", i, to_rs_type(param)).unwrap();
        }
        write!(writer, ")").unwrap();
        if let Some(ret_ty) = function.ty.return_type() {
            write!(writer, " -> {}", to_rs_type(ret_ty)).unwrap();
        }
        writeln!(writer, ";").unwrap();
    }

    let mut globals = Vec::new();

    if let Some(imports) = module.import_section() {
        for import in imports.entries() {
            if let &External::Global(ty) = import.external() {
                let name = import.field().to_string();
                globals.push(Global {
                    is_mutable: ty.is_mutable(),
                    is_pub: true, // Doesn't really apply
                    name,
                    ty: to_rs_type(ty.content_type()),
                    value: String::new(), // Doesn't really apply
                    init_code: None,
                });
            }
        }
    }

    let imported_globals_count = globals.len();

    if let Some(global_section) = module.global_section() {
        for entry in global_section.entries() {
            let ty = entry.global_type();
            let init_val = entry.init_expr().code();
            assert!(init_val.len() >= 1);
            let (value, init_code) = match init_val[0] {
                Opcode::I32Const(c) => (c.to_string(), None),
                Opcode::I64Const(c) => (c.to_string(), None),
                Opcode::F32Const(c) => (c.to_string(), None),
                Opcode::F64Const(c) => (c.to_string(), None),
                _ => (String::from("Default::default()"), Some(init_val)),
            };
            let is_mutable = ty.is_mutable() || init_code.is_some();
            let name = if is_mutable {
                format!("global{}", globals.len())
            } else {
                format!("GLOBAL{}", globals.len())
            };
            globals.push(Global {
                is_mutable,
                is_pub: false,
                name,
                ty: to_rs_type(ty.content_type()),
                value,
                init_code,
            });
        }
    }

    for export in exports.entries() {
        if let &Internal::Global(global_index) = export.internal() {
            let global = &mut globals[global_index as usize];
            global.name = export.field().to_string();
            global.is_pub = true;
        }
    }

    for global in &globals[..imported_globals_count] {
        // if global.is_mutable {
        writeln!(
            writer,
            "    fn {}(&mut self, context: &mut Context<Self::Memory>) -> &mut {};",
            global.name, global.ty
        ).unwrap();
        // } else {
        //     writeln!(writer, "    const {}: {};", global.name, global.ty);
        // }
    }

    writeln!(
        writer,
        "{}",
        r#"}

pub trait Memory {
    fn load8(&mut self, addr: usize) -> u8;
    fn load16(&mut self, addr: usize) -> u16;
    fn load32(&mut self, addr: usize) -> u32;
    fn load64(&mut self, addr: usize) -> u64;

    fn store8(&mut self, addr: usize, val: u8);
    fn store16(&mut self, addr: usize, val: u16);
    fn store32(&mut self, addr: usize, val: u32);
    fn store64(&mut self, addr: usize, val: u64);

    fn store_slice(&mut self, addr: usize, val: &'static [u8]);

    fn grow(&mut self, pages: usize) -> i32;
    fn size(&mut self) -> i32;
}

pub struct Instance<I: Imports<Memory = M>, M: Memory> {
    pub imports: I,
    pub context: Context<M>,
}

pub struct Context<M: Memory> {
    pub memory: M,"#
    ).unwrap();

    for global in &globals[imported_globals_count..] {
        if global.is_mutable {
            write!(writer, "    ").unwrap();
            if global.is_pub {
                write!(writer, "pub ").unwrap();
            }
            writeln!(writer, "{}: {},", global.name, global.ty).unwrap();
        }
    }

    if globals[imported_globals_count..]
        .iter()
        .any(|g| !g.is_mutable)
    {
        writeln!(
            writer,
            "{}",
            r#"}

pub mod consts {"#
        ).unwrap();

        for global in &globals[imported_globals_count..] {
            if !global.is_mutable {
                write!(writer, "    pub").unwrap();
                if !global.is_pub {
                    write!(writer, "(super)").unwrap();
                }
                writeln!(
                    writer,
                    " const {}: {} = {};",
                    global.name, global.ty, global.value
                ).unwrap();
            }
        }
    }

    writeln!(
        writer,
        "{}",
        r#"}

impl<I: Imports<Memory = M>, M: Memory> Instance<I, M> {
    pub fn new(imports: I, mut memory: M) -> Self {"#
    ).unwrap();

    if let Some(memory) = module.memory_section().and_then(|m| m.entries().first()) {
        writeln!(
            writer,
            r#"        let current_pages = memory.size() as usize;
        if current_pages < {0} {{
            memory.grow({0} - current_pages);
            assert_eq!(memory.size(), {0}, "Not enough memory pages allocated");
        }}"#,
            memory.limits().initial()
        ).unwrap();
    }

    if let Some(data_section) = module.data_section() {
        for entry in data_section.entries() {
            let offset = entry.offset().code();
            assert!(offset.len() == 2);
            if let Opcode::I32Const(c) = offset[0] {
                if entry.value().windows(2).all(|a| a[0] == a[1]) {
                    writeln!(
                        writer,
                        r#"        memory.store_slice({}, &[{}; {}]);"#,
                        c,
                        entry.value().first().cloned().unwrap_or_default(),
                        entry.value().len()
                    ).unwrap();
                } else {
                    write!(writer, r#"        memory.store_slice({}, b""#, c).unwrap();
                    for &b in entry.value() {
                        match b {
                            b'\0' => write!(writer, r#"\0"#).unwrap(),
                            b'"' => write!(writer, r#"\""#).unwrap(),
                            b'\\' => write!(writer, r#"\\"#).unwrap(),
                            b'\r' => write!(writer, r#"\r"#).unwrap(),
                            b'\n' => write!(writer, r#"\n"#).unwrap(),
                            b'\t' => write!(writer, r#"\t"#).unwrap(),
                            0x00...0x7F => {
                                write!(writer, "{}", std::char::from_u32(b as _).unwrap()).unwrap()
                            }
                            _ => write!(writer, r#"\x{:X}"#, b).unwrap(),
                        }
                    }
                    writeln!(writer, r#"");"#,).unwrap();
                }
            } else {
                panic!("Data Segment with init expression mismatch");
            }
        }
    }

    writeln!(
        writer,
        "{}",
        r#"        let mut instance = Self {
            imports,
            context: Context {
                memory,"#
    ).unwrap();

    for global in &globals[imported_globals_count..] {
        if global.is_mutable {
            writeln!(writer, "                {}: {},", global.name, global.value).unwrap();
        }
    }

    writeln!(
        writer,
        "{}",
        r#"            },
        };"#
    ).unwrap();

    let has_globals_init_code = globals[imported_globals_count..]
        .iter()
        .any(|g| g.init_code.is_some());

    if has_globals_init_code {
        writeln!(
            writer,
            "        instance.context.init_global_values(&mut instance.imports);"
        ).unwrap();
    }

    if let Some(start) = module.start_section() {
        let name = &functions[start as usize].name;
        writeln!(
            writer,
            "        instance.context.{}(&mut instance.imports);",
            name
        ).unwrap();
    }

    writeln!(
        writer,
        "{}",
        r#"        instance
    }"#
    ).unwrap();

    for export in exports.entries() {
        if let &Internal::Function(fn_index) = export.internal() {
            let function = &functions[fn_index as usize];
            write!(
                writer,
                "    pub fn {}(&mut self",
                mangle_fn_name(export.field())
            ).unwrap();
            for (i, &param) in function.ty.params().iter().enumerate() {
                write!(writer, ", var{}: {}", i, to_rs_type(param)).unwrap();
            }
            write!(writer, ")").unwrap();
            if let Some(ret_ty) = function.ty.return_type() {
                write!(writer, " -> {}", to_rs_type(ret_ty)).unwrap();
            }
            writeln!(writer, " {{").unwrap();
            write!(
                writer,
                "        self.context.{}(&mut self.imports",
                function.name
            ).unwrap();
            for (i, _) in function.ty.params().iter().enumerate() {
                write!(writer, ", var{}", i).unwrap();
            }
            writeln!(writer, ")").unwrap();
            writeln!(writer, "    }}").unwrap();
        }
    }

    let mut indirect_fns = BTreeMap::new();

    if let Some(entry) = module.elements_section().and_then(|e| e.entries().get(0)) {
        let init_val = entry.offset().code();
        assert!(init_val.len() == 2);
        let offset = match init_val[0] {
            Opcode::I32Const(c) => c,
            _ => panic!("Unexpected Element Section Offset"),
        };
        for (fn_ptr, &fn_index) in entry.members().iter().enumerate() {
            let type_index = functions[fn_index as usize].ty_index;
            indirect_fns
                .entry(type_index)
                .or_insert_with(Vec::new)
                .push(((fn_ptr as i32 + offset) as u32, fn_index));
        }
    }

    if opt.public_call_indirect {
        for (&type_index, _) in &indirect_fns {
            let Type::Function(ref fn_type) = types.types()[type_index as usize];
            let fn_name = call_indirect_name(fn_type);
            write!(writer, "    pub fn {}", fn_name).unwrap();
            write!(writer, "(&mut self, ptr: i32").unwrap();
            for (i, &param) in fn_type.params().iter().enumerate() {
                write!(writer, ", var{}: {}", i, to_rs_type(param)).unwrap();
            }
            write!(writer, ")").unwrap();
            if let Some(ret_ty) = fn_type.return_type() {
                write!(writer, " -> {}", to_rs_type(ret_ty)).unwrap();
            }
            write!(
                writer,
                " {{
        self.context.{}(&mut self.imports, ptr",
                fn_name
            ).unwrap();
            for (i, _) in fn_type.params().iter().enumerate() {
                write!(writer, ", var{}", i).unwrap();
            }
            writeln!(
                writer,
                r#")
    }}"#
            ).unwrap();
        }
    }

    writeln!(
        writer,
        "{}",
        r#"}

impl<M: Memory> Context<M> {"#
    ).unwrap();

    if has_globals_init_code {
        writeln!(
            writer,
            "    fn init_global_values<I: Imports<Memory = M>>(&mut self, imports: &mut I) {{"
        ).unwrap();
        for global in &globals[imported_globals_count..] {
            if let Some(ref init_code) = global.init_code {
                writeln!(writer, "        self.{} = {{", global.name).unwrap();
                // TODO Handle Returns in here correctly
                code_builder::build(
                    &mut writer,
                    0,
                    true,
                    import_count,
                    imported_globals_count,
                    &functions,
                    &mut indirect_fns,
                    &globals,
                    types,
                    init_code,
                    3,
                );
                writeln!(writer, ";").unwrap();
            }
        }
        writeln!(writer, "    }}").unwrap();
    }

    for export in exports.entries() {
        if let &Internal::Function(fn_index) = export.internal() {
            let function = &mut functions[fn_index as usize];
            if function.name == export.field() {
                function.make_public = true;
                continue;
            }
            write!(
                writer,
                "    pub fn {}<I: Imports<Memory = M>>(&mut self, imports: &mut I",
                mangle_fn_name(export.field())
            ).unwrap();
            for (i, &param) in function.ty.params().iter().enumerate() {
                write!(writer, ", var{}: {}", i, to_rs_type(param)).unwrap();
            }
            write!(writer, ")").unwrap();
            if let Some(ret_ty) = function.ty.return_type() {
                write!(writer, " -> {}", to_rs_type(ret_ty)).unwrap();
            }
            writeln!(writer, " {{").unwrap();
            write!(writer, "        self.{}(imports", function.name).unwrap();
            for (i, _) in function.ty.params().iter().enumerate() {
                write!(writer, ", var{}", i).unwrap();
            }
            writeln!(writer, ")").unwrap();
            writeln!(writer, "    }}").unwrap();
        }
    }

    for (i, (body, func)) in code.bodies().iter().zip(fns.entries()).enumerate() {
        let type_index = func.type_ref();
        let typ = &types.types()[type_index as usize];
        let fn_type = match *typ {
            Type::Function(ref t) => t,
        };
        let fn_index = import_count + i;
        let function = &functions[fn_index];
        // TODO Ensure there's no collisions with the exports
        if let Some(real_name) = function.real_name {
            writeln!(writer, "    // {}", real_name).unwrap();
        }
        write!(writer, "    ").unwrap();
        if function.make_public {
            write!(writer, "pub ").unwrap();
        }
        write!(
            writer,
            "fn {}<I: Imports<Memory = M>>(&mut self, imports: &mut I",
            function.name
        ).unwrap();
        for (i, &param) in fn_type.params().iter().enumerate() {
            write!(writer, ", mut var{}: {}", i, to_rs_type(param)).unwrap();
        }
        write!(writer, ")").unwrap();
        if let Some(ret_ty) = fn_type.return_type() {
            write!(writer, " -> {}", to_rs_type(ret_ty)).unwrap();
        }
        writeln!(writer, " {{").unwrap();

        let mut expr_index = fn_type.params().len();
        for local in body.locals() {
            let ty = to_rs_type(local.value_type());
            let decimals = if ty.starts_with("f") { ".0" } else { "" };
            for _ in 0..local.count() {
                writeln!(
                    writer,
                    "        let mut var{}: {} = 0{};",
                    expr_index, ty, decimals
                ).unwrap();
                expr_index += 1;
            }
        }

        code_builder::build(
            &mut writer,
            expr_index,
            fn_type.return_type().is_some(),
            import_count,
            imported_globals_count,
            &functions,
            &mut indirect_fns,
            &globals,
            types,
            body.code().elements(),
            2,
        );
    }

    for (type_index, fns) in indirect_fns {
        let Type::Function(ref fn_type) = types.types()[type_index as usize];
        write!(writer, "    ").unwrap();
        if opt.public_call_indirect {
            write!(writer, "pub ").unwrap();
        }
        write!(
            writer,
            "fn {}<I: Imports<Memory = M>>",
            call_indirect_name(fn_type),
        ).unwrap();
        write!(writer, "(&mut self, imports: &mut I, ptr: i32").unwrap();
        for (i, &param) in fn_type.params().iter().enumerate() {
            write!(writer, ", var{}: {}", i, to_rs_type(param)).unwrap();
        }
        write!(writer, ")").unwrap();
        if let Some(ret_ty) = fn_type.return_type() {
            write!(writer, " -> {}", to_rs_type(ret_ty)).unwrap();
        }
        writeln!(
            writer,
            " {{
        match ptr {{"
        ).unwrap();
        for (fn_ptr, fn_index) in fns {
            let function = &functions[fn_index as usize];
            write!(writer, "            {} => ", fn_ptr).unwrap();
            let is_imported = (fn_index as usize) < import_count;
            if is_imported {
                write!(writer, "imports.").unwrap();
            } else {
                write!(writer, "self.").unwrap();
            }
            write!(writer, "{}(", function.name).unwrap();
            if is_imported {
                write!(writer, "self").unwrap();
            } else {
                write!(writer, "imports").unwrap();
            }
            for i in 0..function.ty.params().len() {
                write!(writer, ", var{}", i).unwrap();
            }
            if let Some(real_name) = function.real_name {
                writeln!(writer, "), // {}", real_name).unwrap();
            } else {
                writeln!(writer, "),").unwrap();
            }
        }
        writeln!(
            writer,
            r#"            _ => panic!("Invalid Function Pointer"),
        }}
    }}"#
        ).unwrap();
    }

    writeln!(writer, "}}").unwrap();
}
