extern crate parity_wasm;

use parity_wasm::deserialize_file;
use parity_wasm::elements::{External, FunctionType, ImportCountType, Internal, NameSection,
                            Opcode, Section, Type, ValueType};
use std::collections::BTreeMap;

mod precedence;
mod expr_builder;
mod code_builder;

pub struct Function<'a> {
    name: String,
    ty: &'a FunctionType,
    ty_index: u32,
    real_name: Option<&'a String>,
}

pub struct Global<'a> {
    is_mutable: bool,
    is_pub: bool,
    name: String,
    ty: &'static str,
    value: String,
    init_code: Option<&'a [Opcode]>,
}

fn to_rs_type(t: ValueType) -> &'static str {
    match t {
        ValueType::I32 => "i32",
        ValueType::I64 => "i64",
        ValueType::F32 => "f32",
        ValueType::F64 => "f64",
    }
}

fn print_signature(fn_type: &FunctionType, mut_vars: bool) {
    print!("(&mut self");
    for (i, &param) in fn_type.params().iter().enumerate() {
        print!(",");
        if mut_vars {
            print!(" mut");
        }
        print!(" var{}: {}", i, to_rs_type(param));
    }
    print!(")");
    if let Some(ret_ty) = fn_type.return_type() {
        print!(" -> {}", to_rs_type(ret_ty));
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

fn main() {
    let module = deserialize_file(
        // "wasm-test-bed/target/wasm32-unknown-unknown/release/wasm-test-bed.wasm",
        // "renderer/target/wasm32-unknown-unknown/release/rust-wasm-canvas.wasm",
        // "wasm/funky_karts.wasm",
        "wasm/mono.wasm",
        // "wasmBoy/dist/wasm/index.untouched.wasm",
    ).unwrap();
    let module = module.parse_names().unwrap_or_else(|(_, m)| m);
    // println!(
    //     "{:#?}",
    //     &module.code_section().unwrap().bodies()
    //         [65 - module.import_count(ImportCountType::Function)]
    // );
    // println!("{:#?}", module);
    // return;

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
                });
            }
        }
    }

    for function in fns.entries() {
        let ty_index = function.type_ref();
        let Type::Function(ref fn_type) = types.types()[ty_index as usize];
        let real_name = function_names.and_then(|f| f.names().get(functions.len() as _));
        let name = format!("func{}", functions.len());
        functions.push(Function {
            name,
            ty: fn_type,
            ty_index,
            real_name,
        });
    }

    println!(
        "#![allow(unreachable_code, dead_code, unused_assignments, unused_mut, unused_variables, non_snake_case, non_upper_case_globals, unused_parens)]

pub const PAGE_SIZE: usize = 64 << 10;

pub trait Imports {{"
    );

    for function in &functions[..import_count] {
        print!("    fn {}", function.name);
        print_signature(function.ty, false);
        println!(";");
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

    // TODO Handle imported globals correctly!

    if let Some(global_section) = module.global_section() {
        for entry in global_section.entries() {
            let ty = entry.global_type();
            let name = format!("global{}", globals.len());
            let init_val = entry.init_expr().code();
            assert!(init_val.len() >= 1);
            let (value, init_code) = match init_val[0] {
                Opcode::I32Const(c) => (c.to_string(), None),
                Opcode::I64Const(c) => (c.to_string(), None),
                Opcode::F32Const(c) => (c.to_string(), None),
                Opcode::F64Const(c) => (c.to_string(), None),
                _ => (String::from("Default::default()"), Some(init_val)),
            };
            globals.push(Global {
                is_mutable: ty.is_mutable() || init_code.is_some(),
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
        println!("    fn {}(&mut self) -> &mut {};", global.name, global.ty);
        // } else {
        //     println!("    const {}: {};", global.name, global.ty);
        // }
    }

    println!(
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

    fn store_slice(&mut self, addr: usize, val: &[u8]);

    fn grow(&mut self, pages: usize) -> i32;
    fn size(&self) -> i32;
}

pub struct Wasm<I: Imports, M: Memory> {
    pub imports: I,
    pub mem: M,"#
    );

    for global in &globals[imported_globals_count..] {
        if global.is_mutable {
            print!("    ");
            if global.is_pub {
                print!("pub ");
            }
            println!("{}: {},", global.name, global.ty);
        }
    }

    println!(
        "{}",
        r#"}

impl<I: Imports, M: Memory> Wasm<I, M> {
    pub fn new(imports: I, mut mem: M) -> Self {"#
    );

    if let Some(memory) = module.memory_section().and_then(|m| m.entries().first()) {
        println!(
            r#"        let current_pages = mem.size() as usize;
        if current_pages < {0} {{
            mem.grow({0} - current_pages);
            assert_eq!(mem.size(), {0}, "Not enough memory pages allocated");
        }}"#,
            memory.limits().initial()
        );
    }

    if let Some(data_section) = module.data_section() {
        for entry in data_section.entries() {
            let offset = entry.offset().code();
            assert!(offset.len() == 2);
            if let Opcode::I32Const(c) = offset[0] {
                println!("        mem.store_slice({}, &{:?});", c, entry.value());
            } else {
                panic!("Data Segment with init expression mismatch");
            }
        }
    }

    println!(
        "{}",
        r#"        let mut wasm = Self {
            imports,
            mem,"#
    );

    for global in &globals[imported_globals_count..] {
        if global.is_mutable {
            println!("            {}: {},", global.name, global.value);
        }
    }

    println!("{}", r#"        };"#);

    let has_globals_init_code = globals[imported_globals_count..]
        .iter()
        .any(|g| g.init_code.is_some());

    if has_globals_init_code {
        println!("        wasm.init_global_values();");
    }

    if let Some(start) = module.start_section() {
        let name = &functions[start as usize].name;
        println!("        wasm.{}();", name);
    }

    println!(
        "{}",
        r#"        wasm
    }"#
    );

    for global in &globals[imported_globals_count..] {
        if !global.is_mutable {
            print!("    ");
            if global.is_pub {
                print!("pub ");
            }
            println!("const {}: {} = {};", global.name, global.ty, global.value);
        }
    }

    if has_globals_init_code {
        println!("    fn init_global_values(&mut self) {{");
        for global in &globals[imported_globals_count..] {
            if let Some(ref init_code) = global.init_code {
                println!("        self.{} = {{", global.name);
                code_builder::build(
                    0,
                    true,
                    import_count,
                    imported_globals_count,
                    &functions,
                    &globals,
                    types,
                    init_code,
                    3,
                );
                println!(";");
            }
        }
        println!("    }}");
    }

    for export in exports.entries() {
        if let &Internal::Function(fn_index) = export.internal() {
            let function = &functions[fn_index as usize];
            print!("    pub fn {}", export.field());
            print_signature(function.ty, false);
            println!(" {{");
            print!("        self.{}(", function.name);
            for (i, _) in function.ty.params().iter().enumerate() {
                if i != 0 {
                    print!(", ");
                }
                print!("var{}", i);
            }
            println!(")");
            println!("    }}");
        }
    }

    for (i, (body, func)) in code.bodies().iter().zip(fns.entries()).enumerate() {
        let type_index = func.type_ref();
        let typ = &types.types()[type_index as usize];
        let fn_type = match *typ {
            Type::Function(ref t) => t,
        };
        let fn_index = import_count + i;
        // TODO Ensure there's no collisions with the exports
        if let Some(real_name) = functions[fn_index].real_name {
            println!("    // {}", real_name);
        }
        print!("    fn func{}", fn_index);
        print_signature(fn_type, true);
        println!(" {{");

        let mut expr_index = fn_type.params().len();
        for local in body.locals() {
            let ty = to_rs_type(local.value_type());
            let decimals = if ty.starts_with("f") { ".0" } else { "" };
            for _ in 0..local.count() {
                println!("        let mut var{}: {} = 0{};", expr_index, ty, decimals);
                expr_index += 1;
            }
        }

        code_builder::build(
            expr_index,
            fn_type.return_type().is_some(),
            import_count,
            imported_globals_count,
            &functions,
            &globals,
            types,
            body.code().elements(),
            2,
        );

        println!();
    }

    if let Some(entry) = module.elements_section().and_then(|e| e.entries().get(0)) {
        let mut indirect_fns = BTreeMap::new();
        // TODO Handle offset
        for (fn_ptr, &fn_index) in entry.members().iter().enumerate() {
            let type_index = functions[fn_index as usize].ty_index;
            indirect_fns
                .entry(type_index)
                .or_insert_with(Vec::new)
                .push((fn_ptr, fn_index));
        }

        for (type_index, fns) in indirect_fns {
            let Type::Function(ref fn_type) = types.types()[type_index as usize];
            print!("    fn call_indirect{}", type_index);
            print!("(&mut self, ptr: i32");
            for (i, &param) in fn_type.params().iter().enumerate() {
                print!(", var{}: {}", i, to_rs_type(param));
            }
            print!(")");
            if let Some(ret_ty) = fn_type.return_type() {
                print!(" -> {}", to_rs_type(ret_ty));
            }
            println!(
                " {{
        match ptr {{"
            );
            for (fn_ptr, fn_index) in fns {
                let function = &functions[fn_index as usize];
                print!("            {} => ", fn_ptr);
                if (fn_index as usize) < import_count {
                    print!("self.imports.");
                } else {
                    print!("self.");
                }
                print!("{}(", function.name);
                for i in 0..function.ty.params().len() {
                    if i != 0 {
                        print!(", ");
                    }
                    print!("var{}", i);
                }
                if let Some(real_name) = function.real_name {
                    println!("), // {}", real_name);
                } else {
                    println!("),");
                }
            }
            println!(
                r#"            _ => panic!("Invalid Function Pointer"),
        }}
    }}"#
            );
        }
    }

    println!("}}");
}
