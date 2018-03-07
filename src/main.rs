extern crate parity_wasm;

use parity_wasm::deserialize_file;
use parity_wasm::elements::{BlockType, CodeSection, ExportEntry, ExportSection, External, Func,
                            FuncBody, FunctionSection, FunctionType, ImportCountType, ImportEntry,
                            Internal, Opcode, Type, TypeSection, ValueType};
use std::collections::BTreeMap;

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
    let module = deserialize_file("wasm/livesplit_core.wasm").unwrap();
    let module = module.parse_names().unwrap_or_else(|(_, m)| m);
    // println!("{:#?}", module);
    // return;

    let import_count = module.import_count(ImportCountType::Function);
    let code: &CodeSection = module.code_section().unwrap();
    let fns: &FunctionSection = module.function_section().unwrap();
    let types: &TypeSection = module.type_section().unwrap();
    let exports: &ExportSection = module.export_section().unwrap();

    let mut functions = Vec::new();

    if let Some(imports) = module.import_section() {
        for import in imports.entries() {
            // TODO Handle modules
            let import: &ImportEntry = import;
            if let &External::Function(type_index) = import.external() {
                let typ: &Type = &types.types()[type_index as usize];
                let fn_type = match *typ {
                    Type::Function(ref t) => t,
                };
                functions.push((import.field().to_owned(), fn_type, type_index));
            }
        }
    }

    for function in fns.entries() {
        let type_index = function.type_ref();
        let Type::Function(ref fn_type) = types.types()[type_index as usize];
        let name = format!("func{}", functions.len());
        functions.push((name, fn_type, type_index));
    }

    println!(
        "#![allow(unreachable_code, dead_code, unused_assignments, unused_mut, unused_variables, non_snake_case)]

pub const PAGE_SIZE: usize = 64 << 10;

pub trait Imports {{"
    );

    for &(ref name, fn_type, _) in &functions[..import_count] {
        print!("    fn {}", name);
        print_signature(fn_type, false);
        println!(";");
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

    struct Global {
        is_mutable: bool,
        is_pub: bool,
        name: String,
        ty: &'static str,
        value: String,
    }

    let mut globals = Vec::new();

    if let Some(global_section) = module.global_section() {
        for (i, entry) in global_section.entries().iter().enumerate() {
            let ty = entry.global_type();
            let name = format!("global{}", i);
            let init_val = entry.init_expr().code();
            assert!(init_val.len() == 2);
            let value = match init_val[0] {
                Opcode::I32Const(c) => c.to_string(),
                Opcode::I64Const(c) => c.to_string(),
                Opcode::F32Const(c) => c.to_string(),
                Opcode::F64Const(c) => c.to_string(),
                _ => panic!("Global variable with init expression mismatch"),
            };
            globals.push(Global {
                is_mutable: ty.is_mutable(),
                is_pub: false,
                name,
                ty: to_rs_type(ty.content_type()),
                value,
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

    for global in &globals {
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

    for global in &globals {
        if global.is_mutable {
            println!("            {}: {},", global.name, global.value);
        }
    }

    println!("{}", r#"        };"#);

    if let Some(start) = module.start_section() {
        let name = &functions[start as usize].0;
        println!("        wasm.{}();", name);
    }

    println!(
        "{}",
        r#"        wasm
    }"#
    );

    for global in &globals {
        if !global.is_mutable {
            print!("    ");
            if global.is_pub {
                print!("pub ");
            }
            println!("const {}: {} = {};", global.name, global.ty, global.value);
        }
    }

    for export in exports.entries() {
        let export: &ExportEntry = export;
        if let &Internal::Function(fn_index) = export.internal() {
            let (ref name, ref fn_type, _) = functions[fn_index as usize];
            print!("    pub fn {}", export.field());
            print_signature(fn_type, false);
            println!(" {{");
            print!("        self.{}(", name);
            for (i, _) in fn_type.params().iter().enumerate() {
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
        let body: &FuncBody = body;
        let func: &Func = func;

        let type_index = func.type_ref();
        let typ: &Type = &types.types()[type_index as usize];
        let fn_type = match *typ {
            Type::Function(ref t) => t,
        };
        let fn_index = import_count + i;
        // TODO Ensure there's no collisions with the exports
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

        let mut stack = Vec::new();
        let mut block_types = Vec::new();
        let mut indentation = Indentation(2);
        let mut loop_count = 0;

        block_types.push((
            None,
            if let Some(_) = fn_type.return_type() {
                Some(0)
            } else {
                None
            },
        ));

        for opcode in body.code().elements() {
            // println!("{}// stack: {:?}", indentation, stack);
            use parity_wasm::elements::Opcode::*;
            match *opcode {
                Unreachable => {
                    println!("{}// unreachable", indentation);
                    println!("{}unreachable!();", indentation);
                }
                Nop => {
                    println!("{}// nop", indentation);
                    assert!(stack.is_empty());
                }
                Block(block_type) => {
                    println!("{}// block ({:?})", indentation, block_type);
                    let block_type = if let BlockType::Value(ty) = block_type {
                        println!("{}let var{}: {};", indentation, expr_index, to_rs_type(ty));
                        expr_index += 1;
                        Some(expr_index - 1)
                    } else {
                        None
                    };
                    println!("{}'label{}: loop {{", indentation, loop_count);
                    loop_count += 1;
                    indentation.0 += 1;
                    block_types.push((Some((loop_count - 1, false)), block_type));
                }
                Loop(block_type) => {
                    println!("{}// loop ({:?})", indentation, block_type);
                    let block_type = if let BlockType::Value(ty) = block_type {
                        println!("{}let var{}: {};", indentation, expr_index, to_rs_type(ty));
                        expr_index += 1;
                        Some(expr_index - 1)
                    } else {
                        None
                    };
                    println!("{}'label{}: loop {{", indentation, loop_count);
                    loop_count += 1;
                    indentation.0 += 1;
                    block_types.push((Some((loop_count - 1, true)), block_type));
                }
                If(block_type) => {
                    // TODO Handle Result
                    let expr = stack.pop().unwrap();
                    println!("{}// if ({:?})", indentation, block_type);
                    let block_type = if let BlockType::Value(ty) = block_type {
                        println!("{}let var{}: {};", indentation, expr_index, to_rs_type(ty));
                        expr_index += 1;
                        Some(expr_index - 1)
                    } else {
                        None
                    };
                    println!("{}if var{} != 0 {{", indentation, expr);
                    indentation.0 += 1;
                    block_types.push((None, block_type));
                }
                Else => {
                    let &(_, block_type) = block_types.last().unwrap();
                    if let Some(target_var) = block_type {
                        let expr = stack.pop().unwrap();
                        println!("{}var{} = var{};", indentation, target_var, expr);
                    }
                    indentation.0 -= 1;
                    println!("{}}} else {{ // else", indentation);
                    indentation.0 += 1;
                }
                End => {
                    let (is_loop, block_type) = block_types.pop().unwrap();
                    if is_loop.is_some() {
                        if let Some(target_var) = block_type {
                            // TODO Handle Result
                            if let Some(expr) = stack.pop() {
                                println!("{}var{} = var{};", indentation, target_var, expr);
                                stack.push(target_var);
                                println!("{}break;", indentation);
                            } else {
                                println!("{}// There should've been a loop expression value here, but this may be unreachable", indentation);
                                println!("{}unreachable!()", indentation);
                            }
                        } else {
                            println!("{}break;", indentation);
                        }
                    } else {
                        if !block_types.is_empty() {
                            if let Some(target_var) = block_type {
                                if let Some(expr) = stack.pop() {
                                    println!("{}var{} = var{};", indentation, target_var, expr);
                                    stack.push(target_var);
                                } else {
                                    println!("{}// This seems to be unreachable", indentation);
                                    println!("{}unreachable!()", indentation);
                                }
                            }
                        }
                    }
                    if block_types.is_empty() {
                        if let Some(expr) = stack.pop() {
                            println!("{}var{}", indentation, expr);
                        }
                        assert!(stack.is_empty());
                    }
                    indentation.0 -= 1;
                    println!("{}}} // end", indentation);
                }
                Br(relative_depth) => {
                    let &(loop_info, _) = block_types
                        .iter()
                        .rev()
                        .nth(relative_depth as usize)
                        .unwrap();
                    let (label, is_a_loop) = loop_info.unwrap();

                    println!(
                        "{}// br (depth={}) $label{}",
                        indentation, relative_depth, label
                    );
                    println!(
                        "{}{} 'label{};",
                        indentation,
                        if is_a_loop { "continue" } else { "break" },
                        label
                    );
                }
                BrIf(relative_depth) => {
                    let expr = stack.pop().unwrap();
                    let &(loop_info, _) = block_types
                        .iter()
                        .rev()
                        .nth(relative_depth as usize)
                        .unwrap();
                    let (label, is_a_loop) = loop_info.unwrap();

                    println!(
                        "{}// br_if (depth={}) $label{}",
                        indentation, relative_depth, label
                    );
                    println!("{}if var{} != 0 {{", indentation, expr);
                    println!(
                        "{}    {} 'label{};",
                        indentation,
                        if is_a_loop { "continue" } else { "break" },
                        label
                    );
                    println!("{}}}", indentation);
                }
                BrTable(ref table, default_depth) => {
                    let expr = stack.pop().unwrap();
                    println!(
                        "{}// br_table {:?} default={}",
                        indentation, table, default_depth
                    );
                    println!("{}match var{} {{", indentation, expr);
                    indentation.0 += 1;
                    for (index, &relative_depth) in table.iter().enumerate() {
                        let &(loop_info, _) = block_types
                            .iter()
                            .rev()
                            .nth(relative_depth as usize)
                            .unwrap();
                        let (label, is_a_loop) = loop_info.unwrap();
                        println!(
                            "{}{} => {} 'label{},",
                            indentation,
                            index,
                            if is_a_loop { "continue" } else { "break" },
                            label
                        );
                    }
                    let &(loop_info, _) = block_types
                        .iter()
                        .rev()
                        .nth(default_depth as usize)
                        .unwrap();
                    let (label, is_a_loop) = loop_info.unwrap();
                    println!(
                        "{}_ => {} 'label{},",
                        indentation,
                        if is_a_loop { "continue" } else { "break" },
                        label
                    );
                    indentation.0 -= 1;
                    println!("{}}}", indentation);
                }
                Return => {
                    // TODO Handle returning 0 values
                    println!("{}// return", indentation);
                    if let Some(expr) = stack.pop() {
                        println!("{}return var{};", indentation, expr);
                    } else {
                        println!("{}return;", indentation);
                    }
                }
                Call(fn_index) => {
                    println!("{}// call ${}", indentation, fn_index);
                    let (ref name, fn_type, _) = functions[fn_index as usize];
                    print!("{}", indentation);
                    if fn_type.return_type().is_some() {
                        print!("let var{} = ", expr_index);
                    }
                    if (fn_index as usize) < import_count {
                        print!("self.imports.");
                    } else {
                        print!("self.");
                    }
                    print!("{}(", name);
                    let index = stack.len() - fn_type.params().len();
                    for (i, val) in stack.drain(index..).enumerate() {
                        if i != 0 {
                            print!(", ");
                        }
                        print!("var{}", val);
                    }
                    println!(");");
                    if fn_type.return_type().is_some() {
                        stack.push(expr_index);
                        expr_index += 1;
                    }
                }
                CallIndirect(type_index, _) => {
                    println!("{}// call_indirect $t{}", indentation, type_index);
                    let Type::Function(ref fn_type) = types.types()[type_index as usize];
                    print!("{}", indentation);
                    if fn_type.return_type().is_some() {
                        print!("let var{} = ", expr_index);
                    }
                    let fn_ptr = stack.pop().unwrap();
                    print!("self.call_indirect{}(var{}", type_index, fn_ptr);
                    let index = stack.len() - fn_type.params().len();
                    for val in stack.drain(index..) {
                        print!(", var{}", val);
                    }
                    println!(");");
                    if fn_type.return_type().is_some() {
                        stack.push(expr_index);
                        expr_index += 1;
                    }
                }
                Drop => {
                    println!("{}// drop", indentation);
                    stack.pop().unwrap();
                }
                Select => {
                    let c = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    let a = stack.pop().unwrap();
                    println!("{}// select", indentation);
                    println!(
                        "{}let var{} = if var{} != 0 {{ var{} }} else {{ var{} }};",
                        indentation, expr_index, c, a, b
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                GetLocal(i) => {
                    println!("{}// get_local $var{}", indentation, i);
                    println!("{}let var{} = var{};", indentation, expr_index, i);
                    stack.push(expr_index);
                    expr_index += 1;
                }
                SetLocal(i) => {
                    let val = stack.pop().unwrap();
                    println!("{}// set_local $var{}", indentation, i);
                    println!("{}var{} = var{};", indentation, i, val);
                }
                TeeLocal(i) => {
                    let val = *stack.last().unwrap();
                    println!("{}// tee_local $var{}", indentation, i);
                    println!("{}var{} = var{};", indentation, i, val);
                }
                GetGlobal(i) => {
                    let global = &globals[i as usize];
                    let name = &global.name;
                    let prefix = if global.is_mutable { "self." } else { "Self::" };
                    println!("{}// get_global ${}", indentation, name);
                    println!("{}let var{} = {}{};", indentation, expr_index, prefix, name);
                    stack.push(expr_index);
                    expr_index += 1;
                }
                SetGlobal(i) => {
                    let val = stack.pop().unwrap();
                    let global = &globals[i as usize];
                    let name = &global.name;
                    assert!(global.is_mutable);
                    println!("{}// set_global ${}", indentation, name);
                    println!("{}self.{} = var{};", indentation, name, val);
                }
                I32Load(log_align, offset) => {
                    let addr = stack.pop().unwrap();
                    println!(
                        "{}// i32.load offset={} align={}",
                        indentation,
                        offset,
                        1 << log_align
                    );
                    println!(
                        "{}let var{} = self.mem.load32(var{} as usize + {}) as i32;",
                        indentation, expr_index, addr, offset
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I64Load(log_align, offset) => {
                    let addr = stack.pop().unwrap();
                    println!(
                        "{}// i64.load offset={} align={}",
                        indentation,
                        offset,
                        1 << log_align
                    );
                    println!(
                        "{}let var{} = self.mem.load64(var{} as usize + {}) as i64;",
                        indentation, expr_index, addr, offset
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                F32Load(log_align, offset) => {
                    let addr = stack.pop().unwrap();
                    println!(
                        "{}// f32.load offset={} align={}",
                        indentation,
                        offset,
                        1 << log_align
                    );
                    println!(
                        "{}let var{} = f32::from_bits(self.mem.load32(var{} as usize + {}) as u32);",
                        indentation, expr_index, addr, offset
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                F64Load(log_align, offset) => {
                    let addr = stack.pop().unwrap();
                    println!(
                        "{}// f64.load offset={} align={}",
                        indentation,
                        offset,
                        1 << log_align
                    );
                    println!(
                        "{}let var{} = f64::from_bits(self.mem.load64(var{} as usize + {}) as u64);",
                        indentation, expr_index, addr, offset
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I32Load8S(log_align, offset) => {
                    let addr = stack.pop().unwrap();
                    println!(
                        "{}// i32.load8_s offset={} align={}",
                        indentation,
                        offset,
                        1 << log_align
                    );
                    println!(
                        "{}let var{} = self.mem.load8(var{} as usize + {}) as i8 as i32;",
                        indentation, expr_index, addr, offset
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I32Load8U(log_align, offset) => {
                    let addr = stack.pop().unwrap();
                    println!(
                        "{}// i32.load8_u offset={} align={}",
                        indentation,
                        offset,
                        1 << log_align
                    );
                    println!(
                        "{}let var{} = self.mem.load8(var{} as usize + {}) as i32;",
                        indentation, expr_index, addr, offset
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I32Load16S(log_align, offset) => {
                    let addr = stack.pop().unwrap();
                    println!(
                        "{}// i32.load16_s offset={} align={}",
                        indentation,
                        offset,
                        1 << log_align
                    );
                    println!(
                        "{}let var{} = self.mem.load16(var{} as usize + {}) as i16 as i32;",
                        indentation, expr_index, addr, offset
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I32Load16U(log_align, offset) => {
                    let addr = stack.pop().unwrap();
                    println!(
                        "{}// i32.load16_u offset={} align={}",
                        indentation,
                        offset,
                        1 << log_align
                    );
                    println!(
                        "{}let var{} = self.mem.load16(var{} as usize + {}) as i32;",
                        indentation, expr_index, addr, offset
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I64Load8S(log_align, offset) => {
                    let addr = stack.pop().unwrap();
                    println!(
                        "{}// i64.load8_s offset={} align={}",
                        indentation,
                        offset,
                        1 << log_align
                    );
                    println!(
                        "{}let var{} = self.mem.load8(var{} as usize + {}) as i8 as i64;",
                        indentation, expr_index, addr, offset
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I64Load8U(log_align, offset) => {
                    let addr = stack.pop().unwrap();
                    println!(
                        "{}// i64.load8_u offset={} align={}",
                        indentation,
                        offset,
                        1 << log_align
                    );
                    println!(
                        "{}let var{} = self.mem.load8(var{} as usize + {}) as i64;",
                        indentation, expr_index, addr, offset
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I64Load16S(log_align, offset) => {
                    let addr = stack.pop().unwrap();
                    println!(
                        "{}// i64.load16_s offset={} align={}",
                        indentation,
                        offset,
                        1 << log_align
                    );
                    println!(
                        "{}let var{} = self.mem.load16(var{} as usize + {}) as i16 as i64;",
                        indentation, expr_index, addr, offset
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I64Load16U(log_align, offset) => {
                    let addr = stack.pop().unwrap();
                    println!(
                        "{}// i64.load16_u offset={} align={}",
                        indentation,
                        offset,
                        1 << log_align
                    );
                    println!(
                        "{}let var{} = self.mem.load16(var{} as usize + {}) as i64;",
                        indentation, expr_index, addr, offset
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I64Load32S(log_align, offset) => {
                    let addr = stack.pop().unwrap();
                    println!(
                        "{}// i64.load32_s offset={} align={}",
                        indentation,
                        offset,
                        1 << log_align
                    );
                    println!(
                        "{}let var{} = self.mem.load32(var{} as usize + {}) as i32 as i64;",
                        indentation, expr_index, addr, offset
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I64Load32U(log_align, offset) => {
                    let addr = stack.pop().unwrap();
                    println!(
                        "{}// i64.load32_u offset={} align={}",
                        indentation,
                        offset,
                        1 << log_align
                    );
                    println!(
                        "{}let var{} = self.mem.load32(var{} as usize + {}) as i64;",
                        indentation, expr_index, addr, offset
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I32Store(log_align, offset) => {
                    let value = stack.pop().unwrap();
                    let addr = stack.pop().unwrap();
                    println!(
                        "{}// i32.store offset={} align={}",
                        indentation,
                        offset,
                        1 << log_align
                    );
                    println!(
                        "{}self.mem.store32(var{} as usize + {}, var{} as u32);",
                        indentation, addr, offset, value
                    );
                }
                I64Store(log_align, offset) => {
                    let value = stack.pop().unwrap();
                    let addr = stack.pop().unwrap();
                    println!(
                        "{}// i64.store offset={} align={}",
                        indentation,
                        offset,
                        1 << log_align
                    );
                    println!(
                        "{}self.mem.store64(var{} as usize + {}, var{} as u64);",
                        indentation, addr, offset, value
                    );
                }
                F32Store(log_align, offset) => {
                    let value = stack.pop().unwrap();
                    let addr = stack.pop().unwrap();
                    println!(
                        "{}// f32.store offset={} align={}",
                        indentation,
                        offset,
                        1 << log_align
                    );
                    println!(
                        "{}self.mem.store32(var{} as usize + {}, var{}.to_bits());",
                        indentation, addr, offset, value
                    );
                }
                F64Store(log_align, offset) => {
                    let value = stack.pop().unwrap();
                    let addr = stack.pop().unwrap();
                    println!(
                        "{}// f64.store offset={} align={}",
                        indentation,
                        offset,
                        1 << log_align
                    );
                    println!(
                        "{}self.mem.store64(var{} as usize + {}, var{}.to_bits());",
                        indentation, addr, offset, value
                    );
                }
                I32Store8(log_align, offset) => {
                    let value = stack.pop().unwrap();
                    let addr = stack.pop().unwrap();
                    println!(
                        "{}// i32.store8 offset={} align={}",
                        indentation,
                        offset,
                        1 << log_align
                    );
                    println!(
                        "{}self.mem.store8(var{} as usize + {}, var{} as u8);",
                        indentation, addr, offset, value
                    );
                }
                I32Store16(log_align, offset) => {
                    let value = stack.pop().unwrap();
                    let addr = stack.pop().unwrap();
                    println!(
                        "{}// i32.store16 offset={} align={}",
                        indentation,
                        offset,
                        1 << log_align
                    );
                    println!(
                        "{}self.mem.store16(var{} as usize + {}, var{} as u16);",
                        indentation, addr, offset, value
                    );
                }
                I64Store8(log_align, offset) => {
                    let value = stack.pop().unwrap();
                    let addr = stack.pop().unwrap();
                    println!(
                        "{}// i64.store8 offset={} align={}",
                        indentation,
                        offset,
                        1 << log_align
                    );
                    println!(
                        "{}self.mem.store8(var{} as usize + {}, var{} as u8);",
                        indentation, addr, offset, value
                    );
                }
                I64Store16(log_align, offset) => {
                    let value = stack.pop().unwrap();
                    let addr = stack.pop().unwrap();
                    println!(
                        "{}// i64.store16 offset={} align={}",
                        indentation,
                        offset,
                        1 << log_align
                    );
                    println!(
                        "{}self.mem.store16(var{} as usize + {}, var{} as u16);",
                        indentation, addr, offset, value
                    );
                }
                I64Store32(log_align, offset) => {
                    let value = stack.pop().unwrap();
                    let addr = stack.pop().unwrap();
                    println!(
                        "{}// i64.store32 offset={} align={}",
                        indentation,
                        offset,
                        1 << log_align
                    );
                    println!(
                        "{}self.mem.store32(var{} as usize + {}, var{} as u32);",
                        indentation, addr, offset, value
                    );
                }
                CurrentMemory(_) => {
                    println!("{}// mem.size", indentation);
                    println!("{}let var{} = self.mem.size();", indentation, expr_index);
                    stack.push(expr_index);
                    expr_index += 1;
                }
                GrowMemory(_) => {
                    let pages = stack.pop().unwrap();
                    println!("{}// mem.grow", indentation);
                    println!(
                        "{}let var{} = self.mem.grow(var{} as usize);",
                        indentation, expr_index, pages
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I32Const(c) => {
                    println!("{}// i32.const {}", indentation, c);
                    println!("{}let var{} = {}i32;", indentation, expr_index, c);
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I64Const(c) => {
                    println!("{}// i64.const {}", indentation, c);
                    println!("{}let var{} = {}i64;", indentation, expr_index, c);
                    stack.push(expr_index);
                    expr_index += 1;
                }
                F32Const(c) => {
                    println!("{}// f32.const {}", indentation, c);
                    println!(
                        "{}let var{} = f32::from_bits({}u32);",
                        indentation, expr_index, c as u32
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                F64Const(c) => {
                    println!("{}// f64.const {}", indentation, c);
                    println!(
                        "{}let var{} = f64::from_bits({}u64);",
                        indentation, expr_index, c as u64
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I32Eqz => {
                    let a = stack.pop().unwrap();
                    println!("{}// i32.eqz", indentation);
                    println!(
                        "{}let var{} = (var{} == 0) as i32;",
                        indentation, expr_index, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I32Eq => {
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    println!("{}// i32.eq", indentation);
                    println!(
                        "{}let var{} = (var{} == var{}) as i32;",
                        indentation, expr_index, b, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I32Ne => {
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    println!("{}// i32.ne", indentation);
                    println!(
                        "{}let var{} = (var{} != var{}) as i32;",
                        indentation, expr_index, b, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I32LtS => {
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    println!("{}// i32.lt_s", indentation);
                    println!(
                        "{}let var{} = (var{} < var{}) as i32;",
                        indentation, expr_index, b, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I32LtU => {
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    println!("{}// i32.lt_u", indentation);
                    println!(
                        "{}let var{} = ((var{} as u32) < (var{} as u32)) as i32;",
                        indentation, expr_index, b, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I32GtS => {
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    println!("{}// i32.gt_s", indentation);
                    println!(
                        "{}let var{} = (var{} > var{}) as i32;",
                        indentation, expr_index, b, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I32GtU => {
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    println!("{}// i32.gt_u", indentation);
                    println!(
                        "{}let var{} = ((var{} as u32) > (var{} as u32)) as i32;",
                        indentation, expr_index, b, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I32LeS => {
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    println!("{}// i32.le_s", indentation);
                    println!(
                        "{}let var{} = (var{} <= var{}) as i32;",
                        indentation, expr_index, b, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I32LeU => {
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    println!("{}// i32.le_u", indentation);
                    println!(
                        "{}let var{} = ((var{} as u32) <= (var{} as u32)) as i32;",
                        indentation, expr_index, b, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I32GeS => {
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    println!("{}// i32.ge_s", indentation);
                    println!(
                        "{}let var{} = (var{} >= var{}) as i32;",
                        indentation, expr_index, b, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I32GeU => {
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    println!("{}// i32.ge_u", indentation);
                    println!(
                        "{}let var{} = ((var{} as u32) >= (var{} as u32)) as i32;",
                        indentation, expr_index, b, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I64Eqz => {
                    let a = stack.pop().unwrap();
                    println!("{}// i64.eqz", indentation);
                    println!(
                        "{}let var{} = (var{} == 0) as i32;",
                        indentation, expr_index, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I64Eq => {
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    println!("{}// i64.eq", indentation);
                    println!(
                        "{}let var{} = (var{} == var{}) as i32;",
                        indentation, expr_index, b, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I64Ne => {
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    println!("{}// i64.ne", indentation);
                    println!(
                        "{}let var{} = (var{} != var{}) as i32;",
                        indentation, expr_index, b, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I64LtS => {
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    println!("{}// i64.lt_s", indentation);
                    println!(
                        "{}let var{} = (var{} < var{}) as i32;",
                        indentation, expr_index, b, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I64LtU => {
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    println!("{}// i64.lt_u", indentation);
                    println!(
                        "{}let var{} = ((var{} as u64) < (var{} as u64)) as i32;",
                        indentation, expr_index, b, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I64GtS => {
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    println!("{}// i64.gt_s", indentation);
                    println!(
                        "{}let var{} = (var{} > var{}) as i32;",
                        indentation, expr_index, b, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I64GtU => {
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    println!("{}// i64.gt_u", indentation);
                    println!(
                        "{}let var{} = ((var{} as u64) > (var{} as u64)) as i32;",
                        indentation, expr_index, b, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I64LeS => {
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    println!("{}// i64.le_s", indentation);
                    println!(
                        "{}let var{} = (var{} <= var{}) as i32;",
                        indentation, expr_index, b, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I64LeU => {
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    println!("{}// i64.le_u", indentation);
                    println!(
                        "{}let var{} = ((var{} as u64) <= (var{} as u64)) as i32;",
                        indentation, expr_index, b, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I64GeS => {
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    println!("{}// i64.ge_s", indentation);
                    println!(
                        "{}let var{} = (var{} >= var{}) as i32;",
                        indentation, expr_index, b, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I64GeU => {
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    println!("{}// i64.ge_u", indentation);
                    println!(
                        "{}let var{} = ((var{} as u64) >= (var{} as u64)) as i32;",
                        indentation, expr_index, b, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                F32Eq => {
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    println!("{}// f32.eq", indentation);
                    println!(
                        "{}let var{} = (var{} == var{}) as i32;",
                        indentation, expr_index, b, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                F32Ne => {
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    println!("{}// f32.ne", indentation);
                    println!(
                        "{}let var{} = (var{} != var{}) as i32;",
                        indentation, expr_index, b, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                F32Lt => {
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    println!("{}// f32.lt", indentation);
                    println!(
                        "{}let var{} = (var{} < var{}) as i32;",
                        indentation, expr_index, b, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                F32Gt => {
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    println!("{}// f32.gt", indentation);
                    println!(
                        "{}let var{} = (var{} > var{}) as i32;",
                        indentation, expr_index, b, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                F32Le => {
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    println!("{}// f32.le", indentation);
                    println!(
                        "{}let var{} = (var{} <= var{}) as i32;",
                        indentation, expr_index, b, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                F32Ge => {
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    println!("{}// f32.ge", indentation);
                    println!(
                        "{}let var{} = (var{} >= var{}) as i32;",
                        indentation, expr_index, b, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                F64Eq => {
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    println!("{}// f64.eq", indentation);
                    println!(
                        "{}let var{} = (var{} == var{}) as i32;",
                        indentation, expr_index, b, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                F64Ne => {
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    println!("{}// f64.ne", indentation);
                    println!(
                        "{}let var{} = (var{} != var{}) as i32;",
                        indentation, expr_index, b, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                F64Lt => {
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    println!("{}// f64.lt", indentation);
                    println!(
                        "{}let var{} = (var{} < var{}) as i32;",
                        indentation, expr_index, b, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                F64Gt => {
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    println!("{}// f64.gt", indentation);
                    println!(
                        "{}let var{} = (var{} > var{}) as i32;",
                        indentation, expr_index, b, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                F64Le => {
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    println!("{}// f64.le", indentation);
                    println!(
                        "{}let var{} = (var{} <= var{}) as i32;",
                        indentation, expr_index, b, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                F64Ge => {
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    println!("{}// f64.ge", indentation);
                    println!(
                        "{}let var{} = (var{} >= var{}) as i32;",
                        indentation, expr_index, b, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I32Clz => {
                    let a = stack.pop().unwrap();
                    println!("{}// i32.clz", indentation);
                    println!(
                        "{}let var{} = var{}.leading_zeros() as i32;",
                        indentation, expr_index, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I32Ctz => {
                    let a = stack.pop().unwrap();
                    println!("{}// i32.ctz", indentation);
                    println!(
                        "{}let var{} = var{}.trailing_zeros() as i32;",
                        indentation, expr_index, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I32Popcnt => {
                    let a = stack.pop().unwrap();
                    println!("{}// i32.popcnt", indentation);
                    println!(
                        "{}let var{} = var{}.count_ones() as i32;",
                        indentation, expr_index, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I32Add => {
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    println!("{}// i32.add", indentation);
                    println!(
                        "{}let var{} = var{}.wrapping_add(var{});",
                        indentation, expr_index, b, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I32Sub => {
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    println!("{}// i32.sub", indentation);
                    println!(
                        "{}let var{} = var{}.wrapping_sub(var{});",
                        indentation, expr_index, b, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I32Mul => {
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    println!("{}// i32.mul", indentation);
                    println!(
                        "{}let var{} = var{}.wrapping_mul(var{});",
                        indentation, expr_index, b, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I32DivS => {
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    println!("{}// i32.div_s", indentation);
                    println!(
                        "{}let var{} = var{} / var{};",
                        indentation, expr_index, b, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I32DivU => {
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    println!("{}// i32.div_u", indentation);
                    println!(
                        "{}let var{} = ((var{} as u32) / (var{} as u32)) as i32;",
                        indentation, expr_index, b, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I32RemS => {
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    println!("{}// i32.rem_s", indentation);
                    println!(
                        "{}let var{} = var{}.wrapping_rem(var{});",
                        indentation, expr_index, b, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I32RemU => {
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    println!("{}// i32.rem_u", indentation);
                    println!(
                        "{}let var{} = (var{} as u32).wrapping_rem(var{} as u32) as i32;",
                        indentation, expr_index, b, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I32And => {
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    println!("{}// i32.and", indentation);
                    println!(
                        "{}let var{} = var{} & var{};",
                        indentation, expr_index, b, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I32Or => {
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    println!("{}// i32.or", indentation);
                    println!(
                        "{}let var{} = var{} | var{};",
                        indentation, expr_index, b, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I32Xor => {
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    println!("{}// i32.xor", indentation);
                    println!(
                        "{}let var{} = var{} ^ var{};",
                        indentation, expr_index, b, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I32Shl => {
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    println!("{}// i32.shl", indentation);
                    println!(
                        "{}let var{} = var{}.wrapping_shl(var{} as u32);",
                        indentation, expr_index, b, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I32ShrS => {
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    println!("{}// i32.shr_s", indentation);
                    println!(
                        "{}let var{} = var{}.wrapping_shr(var{} as u32);",
                        indentation, expr_index, b, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I32ShrU => {
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    println!("{}// i32.shr_u", indentation);
                    println!(
                        "{}let var{} = (var{} as u32).wrapping_shr(var{} as u32) as i32;",
                        indentation, expr_index, b, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I32Rotl => {
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    println!("{}// i32.rotl", indentation);
                    println!(
                        "{}let var{} = var{}.rotate_left(var{} as u32);",
                        indentation, expr_index, b, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I32Rotr => {
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    println!("{}// i32.rotr", indentation);
                    println!(
                        "{}let var{} = var{}.rotate_right(var{} as u32);",
                        indentation, expr_index, b, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I64Clz => {
                    let a = stack.pop().unwrap();
                    println!("{}// i64.clz", indentation);
                    println!(
                        "{}let var{} = var{}.leading_zeros() as i64;",
                        indentation, expr_index, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I64Ctz => {
                    let a = stack.pop().unwrap();
                    println!("{}// i64.ctz", indentation);
                    println!(
                        "{}let var{} = var{}.trailing_zeros() as i64;",
                        indentation, expr_index, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I64Popcnt => {
                    let a = stack.pop().unwrap();
                    println!("{}// i64.popcnt", indentation);
                    println!(
                        "{}let var{} = var{}.count_ones() as i64;",
                        indentation, expr_index, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I64Add => {
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    println!("{}// i64.add", indentation);
                    println!(
                        "{}let var{} = var{}.wrapping_add(var{});",
                        indentation, expr_index, b, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I64Sub => {
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    println!("{}// i64.sub", indentation);
                    println!(
                        "{}let var{} = var{}.wrapping_sub(var{});",
                        indentation, expr_index, b, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I64Mul => {
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    println!("{}// i64.mul", indentation);
                    println!(
                        "{}let var{} = var{}.wrapping_mul(var{});",
                        indentation, expr_index, b, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I64DivS => {
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    println!("{}// i64.div_s", indentation);
                    println!(
                        "{}let var{} = var{} / var{};",
                        indentation, expr_index, b, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I64DivU => {
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    println!("{}// i64.div_u", indentation);
                    println!(
                        "{}let var{} = ((var{} as u64) / (var{} as u64)) as i64;",
                        indentation, expr_index, b, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I64RemS => {
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    println!("{}// i64.rem_s", indentation);
                    println!(
                        "{}let var{} = var{}.wrapping_rem(var{});",
                        indentation, expr_index, b, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I64RemU => {
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    println!("{}// i64.rem_u", indentation);
                    println!(
                        "{}let var{} = (var{} as u64).wrapping_rem(var{} as u64) as i64;",
                        indentation, expr_index, b, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I64And => {
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    println!("{}// i64.and", indentation);
                    println!(
                        "{}let var{} = var{} & var{};",
                        indentation, expr_index, b, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I64Or => {
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    println!("{}// i64.or", indentation);
                    println!(
                        "{}let var{} = var{} | var{};",
                        indentation, expr_index, b, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I64Xor => {
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    println!("{}// i64.xor", indentation);
                    println!(
                        "{}let var{} = var{} ^ var{};",
                        indentation, expr_index, b, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I64Shl => {
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    println!("{}// i64.shl", indentation);
                    println!(
                        "{}let var{} = var{}.wrapping_shl(var{} as u32);",
                        indentation, expr_index, b, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I64ShrS => {
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    println!("{}// i64.shr_s", indentation);
                    println!(
                        "{}let var{} = var{}.wrapping_shr(var{} as u32);",
                        indentation, expr_index, b, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I64ShrU => {
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    println!("{}// i64.shr_u", indentation);
                    println!(
                        "{}let var{} = (var{} as u64).wrapping_shr(var{} as u32) as i64;",
                        indentation, expr_index, b, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I64Rotl => {
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    println!("{}// i64.rotl", indentation);
                    println!(
                        "{}let var{} = var{}.rotate_left(var{} as u32);",
                        indentation, expr_index, b, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I64Rotr => {
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    println!("{}// i64.rotr", indentation);
                    println!(
                        "{}let var{} = var{}.rotate_right(var{} as u32);",
                        indentation, expr_index, b, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                F32Abs => {
                    let a = stack.pop().unwrap();
                    println!("{}// f32.abs", indentation);
                    println!(
                        "{}let var{} = f32::from_bits(var{}.to_bits() & 0x7FFF_FFFF);",
                        indentation, expr_index, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                F32Neg => {
                    let a = stack.pop().unwrap();
                    println!("{}// f32.neg", indentation);
                    println!(
                        "{}let var{} = f32::from_bits(var{}.to_bits() ^ 0x8000_0000);",
                        indentation, expr_index, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                F32Ceil => {
                    let a = stack.pop().unwrap();
                    println!("{}// f32.ceil", indentation);
                    println!("{}let var{} = var{}.ceil();", indentation, expr_index, a);
                    stack.push(expr_index);
                    expr_index += 1;
                }
                F32Floor => {
                    let a = stack.pop().unwrap();
                    println!("{}// f32.floor", indentation);
                    println!("{}let var{} = var{}.floor();", indentation, expr_index, a);
                    stack.push(expr_index);
                    expr_index += 1;
                }
                F32Trunc => {
                    let a = stack.pop().unwrap();
                    println!("{}// f32.trunc", indentation);
                    println!("{}let var{} = var{}.trunc();", indentation, expr_index, a);
                    stack.push(expr_index);
                    expr_index += 1;
                }
                F32Nearest => {
                    let a = stack.pop().unwrap();
                    println!("{}// f32.nearest", indentation);
                    println!(
                        "{0}let var{1} = {{
{0}    let round = var{2}.round();
{0}    if var{2}.fract().abs() != 0.5 {{
{0}        round
{0}    }} else if round % 2.0 == 1.0 {{
{0}        var{2}.floor()
{0}    }} else if round % 2.0 == -1.0 {{
{0}        var{2}.ceil()
{0}    }} else {{
{0}        round
{0}    }}
{0}}};",
                        indentation, expr_index, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                F32Sqrt => {
                    let a = stack.pop().unwrap();
                    println!("{}// f32.sqrt", indentation);
                    println!("{}let var{} = var{}.sqrt();", indentation, expr_index, a);
                    stack.push(expr_index);
                    expr_index += 1;
                }
                F32Add => {
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    println!("{}// f32.add", indentation);
                    println!(
                        "{}let var{} = var{} + var{};",
                        indentation, expr_index, b, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                F32Sub => {
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    println!("{}// f32.sub", indentation);
                    println!(
                        "{}let var{} = var{} - var{};",
                        indentation, expr_index, b, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                F32Mul => {
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    println!("{}// f32.mul", indentation);
                    println!(
                        "{}let var{} = var{} * var{};",
                        indentation, expr_index, b, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                F32Div => {
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    println!("{}// f32.div", indentation);
                    println!(
                        "{}let var{} = var{} / var{};",
                        indentation, expr_index, b, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                F32Min => {
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    println!("{}// f32.min", indentation);
                    println!(
                        "{0}let var{1} = if var{2}.is_nan() || var{3}.is_nan() {{ var{2} }} else {{ var{2}.min(var{3}) }};",
                        indentation, expr_index, b, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                F32Max => {
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    println!("{}// f32.max", indentation);
                    println!(
                        "{0}let var{1} = if var{2}.is_nan() || var{3}.is_nan() {{ var{2} }} else {{ var{2}.max(var{3}) }};",
                        indentation, expr_index, b, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                F32Copysign => {
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    println!("{}// f32.copysign", indentation);
                    println!(
                        "{}let var{} = f32::from_bits((var{}.to_bits() & !(1 << 31)) | (var{}.to_bits() & (1 << 31)));",
                        indentation, expr_index, b, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                F64Abs => {
                    let a = stack.pop().unwrap();
                    println!("{}// f64.abs", indentation);
                    println!(
                        "{}let var{} = f64::from_bits(var{}.to_bits() & 0x7FFF_FFFF_FFFF_FFFF);",
                        indentation, expr_index, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                F64Neg => {
                    let a = stack.pop().unwrap();
                    println!("{}// f64.neg", indentation);
                    println!(
                        "{}let var{} = f64::from_bits(var{}.to_bits() ^ 0x8000_0000_0000_0000);",
                        indentation, expr_index, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                F64Ceil => {
                    let a = stack.pop().unwrap();
                    println!("{}// f64.ceil", indentation);
                    println!("{}let var{} = var{}.ceil();", indentation, expr_index, a);
                    stack.push(expr_index);
                    expr_index += 1;
                }
                F64Floor => {
                    let a = stack.pop().unwrap();
                    println!("{}// f64.floor", indentation);
                    println!("{}let var{} = var{}.floor();", indentation, expr_index, a);
                    stack.push(expr_index);
                    expr_index += 1;
                }
                F64Trunc => {
                    let a = stack.pop().unwrap();
                    println!("{}// f64.trunc", indentation);
                    println!("{}let var{} = var{}.trunc();", indentation, expr_index, a);
                    stack.push(expr_index);
                    expr_index += 1;
                }
                F64Nearest => {
                    let a = stack.pop().unwrap();
                    println!("{}// f64.nearest", indentation);
                    println!(
                        "{0}let var{1} = {{
{0}    let round = var{2}.round();
{0}    if var{2}.fract().abs() != 0.5 {{
{0}        round
{0}    }} else if round % 2.0 == 1.0 {{
{0}        var{2}.floor()
{0}    }} else if round % 2.0 == -1.0 {{
{0}        var{2}.ceil()
{0}    }} else {{
{0}        round
{0}    }}
{0}}};",
                        indentation, expr_index, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                F64Sqrt => {
                    let a = stack.pop().unwrap();
                    println!("{}// f64.sqrt", indentation);
                    println!("{}let var{} = var{}.sqrt();", indentation, expr_index, a);
                    stack.push(expr_index);
                    expr_index += 1;
                }
                F64Add => {
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    println!("{}// f64.add", indentation);
                    println!(
                        "{}let var{} = var{} + var{};",
                        indentation, expr_index, b, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                F64Sub => {
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    println!("{}// f64.sub", indentation);
                    println!(
                        "{}let var{} = var{} - var{};",
                        indentation, expr_index, b, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                F64Mul => {
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    println!("{}// f64.mul", indentation);
                    println!(
                        "{}let var{} = var{} * var{};",
                        indentation, expr_index, b, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                F64Div => {
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    println!("{}// f64.div", indentation);
                    println!(
                        "{}let var{} = var{} / var{};",
                        indentation, expr_index, b, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                F64Min => {
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    println!("{}// f64.min", indentation);
                    println!(
                        "{0}let var{1} = if var{2}.is_nan() || var{3}.is_nan() {{ var{2} }} else {{ var{2}.min(var{3}) }};",
                        indentation, expr_index, b, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                F64Max => {
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    println!("{}// f64.max", indentation);
                    println!(
                        "{0}let var{1} = if var{2}.is_nan() || var{3}.is_nan() {{ var{2} }} else {{ var{2}.max(var{3}) }};",
                        indentation, expr_index, b, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                F64Copysign => {
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    println!("{}// f64.copysign", indentation);
                    println!(
                        "{}let var{} = f64::from_bits((var{}.to_bits() & !(1 << 63)) | (var{}.to_bits() & (1 << 63)));",
                        indentation, expr_index, b, a
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I32WrapI64 => {
                    let val = stack.pop().unwrap();
                    println!("{}// i32.wrap/i64", indentation);
                    println!("{}let var{} = var{} as i32;", indentation, expr_index, val);
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I32TruncSF32 => {
                    let val = stack.pop().unwrap();
                    println!("{}// i32.trunc_s/f32", indentation);
                    println!("{}let var{} = var{} as i32;", indentation, expr_index, val);
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I32TruncUF32 => {
                    let val = stack.pop().unwrap();
                    println!("{}// i32.trunc_u/f32", indentation);
                    println!(
                        "{}let var{} = var{} as u32 as i32;",
                        indentation, expr_index, val
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I32TruncSF64 => {
                    let val = stack.pop().unwrap();
                    println!("{}// i32.trunc_s/f64", indentation);
                    println!("{}let var{} = var{} as i32;", indentation, expr_index, val);
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I32TruncUF64 => {
                    let val = stack.pop().unwrap();
                    println!("{}// i32.trunc_u/f64", indentation);
                    println!(
                        "{}let var{} = var{} as u32 as i32;",
                        indentation, expr_index, val
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I64ExtendSI32 => {
                    let val = stack.pop().unwrap();
                    println!("{}// i64.extend_s/i32", indentation);
                    println!("{}let var{} = var{} as i64;", indentation, expr_index, val);
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I64ExtendUI32 => {
                    let val = stack.pop().unwrap();
                    println!("{}// i64.extend_u/i32", indentation);
                    println!(
                        "{}let var{} = var{} as u32 as i64;",
                        indentation, expr_index, val
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I64TruncSF32 => {
                    let val = stack.pop().unwrap();
                    println!("{}// i64.trunc_s/f32", indentation);
                    println!("{}let var{} = var{} as i64;", indentation, expr_index, val);
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I64TruncUF32 => {
                    let val = stack.pop().unwrap();
                    println!("{}// i64.trunc_u/f32", indentation);
                    println!(
                        "{}let var{} = var{} as u64 as i64;",
                        indentation, expr_index, val
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I64TruncSF64 => {
                    let val = stack.pop().unwrap();
                    println!("{}// i64.trunc_s/f64", indentation);
                    println!("{}let var{} = var{} as i64;", indentation, expr_index, val);
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I64TruncUF64 => {
                    let val = stack.pop().unwrap();
                    println!("{}// i64.trunc_u/f64", indentation);
                    println!(
                        "{}let var{} = var{} as u64 as i64;",
                        indentation, expr_index, val
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                F32ConvertSI32 => {
                    let val = stack.pop().unwrap();
                    println!("{}// f32.convert_s/i32", indentation);
                    println!("{}let var{} = var{} as f32;", indentation, expr_index, val);
                    stack.push(expr_index);
                    expr_index += 1;
                }
                F32ConvertUI32 => {
                    let val = stack.pop().unwrap();
                    println!("{}// f32.convert_u/i32", indentation);
                    println!(
                        "{}let var{} = var{} as u32 as f32;",
                        indentation, expr_index, val
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                F32ConvertSI64 => {
                    let val = stack.pop().unwrap();
                    println!("{}// f32.convert_s/i64", indentation);
                    println!("{}let var{} = var{} as f32;", indentation, expr_index, val);
                    stack.push(expr_index);
                    expr_index += 1;
                }
                F32ConvertUI64 => {
                    let val = stack.pop().unwrap();
                    println!("{}// f32.convert_u/i64", indentation);
                    println!(
                        "{}let var{} = var{} as u64 as f32;",
                        indentation, expr_index, val
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                F32DemoteF64 => {
                    let val = stack.pop().unwrap();
                    println!("{}// f32.demote/f64", indentation);
                    println!("{}let var{} = var{} as f32;", indentation, expr_index, val);
                    stack.push(expr_index);
                    expr_index += 1;
                }
                F64ConvertSI32 => {
                    let val = stack.pop().unwrap();
                    println!("{}// f64.convert_s/i32", indentation);
                    println!("{}let var{} = var{} as f64;", indentation, expr_index, val);
                    stack.push(expr_index);
                    expr_index += 1;
                }
                F64ConvertUI32 => {
                    let val = stack.pop().unwrap();
                    println!("{}// f64.convert_u/i32", indentation);
                    println!(
                        "{}let var{} = var{} as u32 as f64;",
                        indentation, expr_index, val
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                F64ConvertSI64 => {
                    let val = stack.pop().unwrap();
                    println!("{}// f64.convert_s/i64", indentation);
                    println!("{}let var{} = var{} as f64;", indentation, expr_index, val);
                    stack.push(expr_index);
                    expr_index += 1;
                }
                F64ConvertUI64 => {
                    let val = stack.pop().unwrap();
                    println!("{}// f64.convert_u/i64", indentation);
                    println!(
                        "{}let var{} = var{} as u64 as f64;",
                        indentation, expr_index, val
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                F64PromoteF32 => {
                    let val = stack.pop().unwrap();
                    println!("{}// f64.promote/f32", indentation);
                    println!("{}let var{} = var{} as f64;", indentation, expr_index, val);
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I32ReinterpretF32 => {
                    let val = stack.pop().unwrap();
                    println!("{}// i32.reinterpret/f32", indentation);
                    println!(
                        "{}let var{} = var{}.to_bits() as i32;",
                        indentation, expr_index, val
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                I64ReinterpretF64 => {
                    let val = stack.pop().unwrap();
                    println!("{}// i64.reinterpret/f64", indentation);
                    println!(
                        "{}let var{} = var{}.to_bits() as i64;",
                        indentation, expr_index, val
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                F32ReinterpretI32 => {
                    let val = stack.pop().unwrap();
                    println!("{}// f32.reinterpret/i32", indentation);
                    println!(
                        "{}let var{} = f32::from_bits(var{} as u32);",
                        indentation, expr_index, val
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
                F64ReinterpretI64 => {
                    let val = stack.pop().unwrap();
                    println!("{}// f64.reinterpret/i64", indentation);
                    println!(
                        "{}let var{} = f64::from_bits(var{} as u64);",
                        indentation, expr_index, val
                    );
                    stack.push(expr_index);
                    expr_index += 1;
                }
            }
        }
    }

    if let Some(entry) = module.elements_section().and_then(|e| e.entries().get(0)) {
        let mut indirect_fns = BTreeMap::new();
        // TODO Handle offset
        for (fn_ptr, &fn_index) in entry.members().iter().enumerate() {
            let type_index = functions[fn_index as usize].2;
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
                let (ref name, fn_type, _) = functions[fn_index as usize];
                print!("            {} => ", fn_ptr);
                if (fn_index as usize) < import_count {
                    print!("self.imports.");
                } else {
                    print!("self.");
                }
                print!("{}(", name);
                for i in 0..fn_type.params().len() {
                    if i != 0 {
                        print!(", ");
                    }
                    print!("var{}", i);
                }
                println!("),");
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
