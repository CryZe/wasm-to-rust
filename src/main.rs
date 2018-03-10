extern crate parity_wasm;

use parity_wasm::deserialize_file;
use parity_wasm::elements::{BlockType, CodeSection, ExportEntry, ExportSection, External, Func,
                            FuncBody, FunctionSection, FunctionType, ImportCountType, ImportEntry,
                            Internal, NameSection, Opcode, Section, Type, TypeSection, ValueType};
use std::collections::BTreeMap;

mod precedence;
mod expr_builder;

use expr_builder::ExprBuilder;

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
        // "wasm-test-bed/target/wasm32-unknown-unknown/release/wasm_test_bed.wasm",
        // "renderer/target/wasm32-unknown-unknown/release/rust-wasm-canvas.wasm",
        "wasm/livesplit_core.wasm",
        // "wasmBoy/dist/wasm/index.untouched.wasm",
    ).unwrap();
    let module = module.parse_names().unwrap_or_else(|(_, m)| m);
    // println!("{:#?}", &module.code_section().unwrap().bodies()[87 - module.import_count(ImportCountType::Function)]);
    // println!("{:?}", module);
    // return;

    let import_count = module.import_count(ImportCountType::Function);
    let code: &CodeSection = module.code_section().unwrap();
    let fns: &FunctionSection = module.function_section().unwrap();
    let types: &TypeSection = module.type_section().unwrap();
    let exports: &ExportSection = module.export_section().unwrap();
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
            let import: &ImportEntry = import;
            if let &External::Function(type_index) = import.external() {
                let typ: &Type = &types.types()[type_index as usize];
                let fn_type = match *typ {
                    Type::Function(ref t) => t,
                };
                functions.push((import.field().to_owned(), fn_type, type_index, None));
            }
        }
    }

    for function in fns.entries() {
        let type_index = function.type_ref();
        let Type::Function(ref fn_type) = types.types()[type_index as usize];
        let real_name = function_names.and_then(|f| f.names().get(functions.len() as _));
        let name = format!("func{}", functions.len());
        functions.push((name, fn_type, type_index, real_name));
    }

    println!(
        "#![allow(unreachable_code, dead_code, unused_assignments, unused_mut, unused_variables, non_snake_case, non_upper_case_globals, unused_parens)]

pub const PAGE_SIZE: usize = 64 << 10;

pub trait Imports {{"
    );

    for &(ref name, fn_type, ..) in &functions[..import_count] {
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
            let (ref name, ref fn_type, ..) = functions[fn_index as usize];
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
        if let Some(real_name) = functions[fn_index].3 {
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

        // TODO Type Inference
        let mut expr_builder = ExprBuilder::new();
        let mut block_types = Vec::new();
        let mut indentation = Indentation(2);
        let mut loop_count = 0;

        block_types.push((
            None,
            if let Some(_) = fn_type.return_type() {
                Some((precedence::PATH, String::new()))
            } else {
                None
            },
        ));

        for opcode in body.code().elements() {
            // println!("{}// stack: {:?}", indentation, stack);
            use parity_wasm::elements::Opcode::*;
            match *opcode {
                Unreachable => {
                    println!("{}unreachable!();", indentation);
                }
                Nop => {
                    assert!(expr_builder.is_empty());
                }
                Block(block_type) => {
                    let block_type = if let BlockType::Value(ty) = block_type {
                        let var_name = format!("var{}", expr_index);
                        println!("{}let {}: {};", indentation, var_name, to_rs_type(ty));
                        expr_index += 1;
                        Some((precedence::PATH, var_name))
                    } else {
                        None
                    };
                    println!("{}'label{}: loop {{", indentation, loop_count);
                    loop_count += 1;
                    indentation.0 += 1;
                    block_types.push((Some((loop_count - 1, false)), block_type));
                }
                Loop(block_type) => {
                    let block_type = if let BlockType::Value(ty) = block_type {
                        let dst = format!("var{}", expr_index);
                        println!("{}let {}: {};", indentation, dst, to_rs_type(ty));
                        expr_index += 1;
                        Some((precedence::PATH, dst))
                    } else {
                        None
                    };
                    println!("{}'label{}: loop {{", indentation, loop_count);
                    loop_count += 1;
                    indentation.0 += 1;
                    block_types.push((Some((loop_count - 1, true)), block_type));
                }
                If(block_type) => {
                    let expr = expr_builder.pop_formatted(precedence::COMPARISON).unwrap();
                    let block_type = if let BlockType::Value(ty) = block_type {
                        let dst = format!("var{}", expr_index);
                        println!("{}let {}: {};", indentation, dst, to_rs_type(ty));
                        expr_index += 1;
                        Some((precedence::PATH, dst))
                    } else {
                        None
                    };
                    println!("{}if {} != 0 {{", indentation, expr);
                    indentation.0 += 1;
                    block_types.push((None, block_type));
                }
                Else => {
                    let &(_, ref block_type) = block_types.last().unwrap();
                    if let &Some((_, ref target_var)) = block_type {
                        let (_, expr) = expr_builder.pop().unwrap();
                        println!("{}{} = {};", indentation, target_var, expr);
                    }
                    indentation.0 -= 1;
                    println!("{}}} else {{", indentation);
                    indentation.0 += 1;
                }
                End => {
                    let (is_loop, block_type) = block_types.pop().unwrap();
                    if is_loop.is_some() {
                        if let Some((precedence, target_var)) = block_type {
                            // TODO Handle Result
                            if let Some((_, expr)) = expr_builder.pop() {
                                println!("{}{} = {};", indentation, target_var, expr);
                                expr_builder.push((precedence, target_var));
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
                            if let Some((precedence, target_var)) = block_type {
                                if let Some((_, expr)) = expr_builder.pop() {
                                    println!("{}{} = {};", indentation, target_var, expr);
                                    expr_builder.push((precedence, target_var));
                                } else {
                                    println!("{}// This seems to be unreachable", indentation);
                                    println!("{}unreachable!()", indentation);
                                }
                            }
                        }
                    }
                    if block_types.is_empty() {
                        if let Some((_, expr)) = expr_builder.pop() {
                            println!("{}{}", indentation, expr);
                        }
                        assert!(expr_builder.is_empty());
                    }
                    indentation.0 -= 1;
                    println!("{}}}", indentation);
                }
                Br(relative_depth) => {
                    let &(loop_info, _) = block_types
                        .iter()
                        .rev()
                        .nth(relative_depth as usize)
                        .unwrap();
                    let (label, is_a_loop) = loop_info.unwrap();

                    println!(
                        "{}{} 'label{};",
                        indentation,
                        if is_a_loop { "continue" } else { "break" },
                        label
                    );
                }
                BrIf(relative_depth) => {
                    let expr = expr_builder.pop_formatted(precedence::COMPARISON).unwrap();
                    let &(loop_info, _) = block_types
                        .iter()
                        .rev()
                        .nth(relative_depth as usize)
                        .unwrap();
                    let (label, is_a_loop) = loop_info.unwrap();

                    println!("{}if {} != 0 {{", indentation, expr);
                    println!(
                        "{}    {} 'label{};",
                        indentation,
                        if is_a_loop { "continue" } else { "break" },
                        label
                    );
                    println!("{}}}", indentation);
                }
                BrTable(ref table, default_depth) => {
                    let (_, expr) = expr_builder.pop().unwrap();
                    println!("{}match {} {{", indentation, expr);
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
                    if let Some((_, expr)) = expr_builder.pop() {
                        println!("{}return {};", indentation, expr);
                    } else {
                        println!("{}return;", indentation);
                    }
                }
                Call(fn_index) => {
                    let (ref name, fn_type, _, real_name) = functions[fn_index as usize];
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
                    let index = expr_builder.len() - fn_type.params().len();
                    for (i, (_, expr)) in expr_builder.inner().drain(index..).enumerate() {
                        if i != 0 {
                            print!(", ");
                        }
                        print!("{}", expr);
                    }
                    if let Some(real_name) = real_name {
                        println!("); // {}", real_name);
                    } else {
                        println!(");");
                    }
                    if fn_type.return_type().is_some() {
                        expr_builder.push((precedence::PATH, format!("var{}", expr_index)));
                        expr_index += 1;
                    }
                }
                CallIndirect(type_index, _) => {
                    let Type::Function(ref fn_type) = types.types()[type_index as usize];
                    print!("{}", indentation);
                    if fn_type.return_type().is_some() {
                        print!("let var{} = ", expr_index);
                    }
                    let (_, fn_ptr) = expr_builder.pop().unwrap();
                    print!("self.call_indirect{}({}", type_index, fn_ptr);
                    let index = expr_builder.len() - fn_type.params().len();
                    for (_, expr) in expr_builder.inner().drain(index..) {
                        print!(", {}", expr);
                    }
                    println!(");");
                    if fn_type.return_type().is_some() {
                        expr_builder.push((precedence::PATH, format!("var{}", expr_index)));
                        expr_index += 1;
                    }
                }
                Drop => {
                    expr_builder.pop().unwrap();
                }
                Select => {
                    // TODO Should not be short circuiting
                    let c = expr_builder.pop_formatted(precedence::COMPARISON).unwrap();
                    let (_, b) = expr_builder.pop().unwrap();
                    let (_, a) = expr_builder.pop().unwrap();
                    expr_builder.push((
                        precedence::PATH,
                        format!("(if {} != 0 {{ {} }} else {{ {} }})", c, a, b),
                    ));
                }
                GetLocal(i) => {
                    // Can't be inline in an expression since it may be
                    // overwritten until it's used.
                    let dst = format!("var{}", expr_index);
                    println!("{}let {} = var{};", indentation, dst, i);
                    expr_index += 1;
                    expr_builder.push((precedence::PATH, dst));
                }
                SetLocal(i) => {
                    let (_, expr) = expr_builder.pop().unwrap();
                    println!("{}var{} = {};", indentation, i, expr);
                }
                TeeLocal(i) => {
                    let (_, expr) = expr_builder.pop().unwrap();
                    println!("{}var{} = {};", indentation, i, expr);
                    let dst = format!("var{}", expr_index);
                    println!("{}let {} = var{};", indentation, dst, i);
                    expr_index += 1;
                    expr_builder.push((precedence::PATH, dst));
                }
                GetGlobal(i) => {
                    let global = &globals[i as usize];
                    let name = &global.name;
                    if global.is_mutable {
                        let dst = format!("var{}", expr_index);
                        println!("{}let {} = self.{};", indentation, dst, name);
                        expr_index += 1;
                        expr_builder.push((precedence::PATH, dst));
                    } else {
                        expr_builder.push((precedence::PATH, format!("Self::{}", name)));
                    }
                }
                SetGlobal(i) => {
                    let (_, expr) = expr_builder.pop().unwrap();
                    let global = &globals[i as usize];
                    let name = &global.name;
                    assert!(global.is_mutable);
                    println!("{}self.{} = {};", indentation, name, expr);
                }
                I32Load(_log_align, offset) => {
                    let addr = expr_builder.pop_formatted(precedence::AS).unwrap();
                    println!(
                        "{}let var{} = self.mem.load32({} as usize{}) as i32;",
                        indentation,
                        expr_index,
                        addr,
                        if offset != 0 {
                            format!(" + {}", offset)
                        } else {
                            String::new()
                        }
                    );
                    expr_builder.push((precedence::PATH, format!("var{}", expr_index)));
                    expr_index += 1;
                }
                I64Load(_log_align, offset) => {
                    let addr = expr_builder.pop_formatted(precedence::AS).unwrap();
                    println!(
                        "{}let var{} = self.mem.load64({} as usize{}) as i64;",
                        indentation,
                        expr_index,
                        addr,
                        if offset != 0 {
                            format!(" + {}", offset)
                        } else {
                            String::new()
                        }
                    );
                    expr_builder.push((precedence::PATH, format!("var{}", expr_index)));
                    expr_index += 1;
                }
                F32Load(_log_align, offset) => {
                    let addr = expr_builder.pop_formatted(precedence::AS).unwrap();
                    println!(
                        "{}let var{} = f32::from_bits(self.mem.load32({} as usize{}));",
                        indentation,
                        expr_index,
                        addr,
                        if offset != 0 {
                            format!(" + {}", offset)
                        } else {
                            String::new()
                        }
                    );
                    expr_builder.push((precedence::PATH, format!("var{}", expr_index)));
                    expr_index += 1;
                }
                F64Load(_log_align, offset) => {
                    let addr = expr_builder.pop_formatted(precedence::AS).unwrap();
                    println!(
                        "{}let var{} = f64::from_bits(self.mem.load64({} as usize{}));",
                        indentation,
                        expr_index,
                        addr,
                        if offset != 0 {
                            format!(" + {}", offset)
                        } else {
                            String::new()
                        }
                    );
                    expr_builder.push((precedence::PATH, format!("var{}", expr_index)));
                    expr_index += 1;
                }
                I32Load8S(_log_align, offset) => {
                    let addr = expr_builder.pop_formatted(precedence::AS).unwrap();
                    println!(
                        "{}let var{} = self.mem.load8({} as usize{}) as i8 as i32;",
                        indentation,
                        expr_index,
                        addr,
                        if offset != 0 {
                            format!(" + {}", offset)
                        } else {
                            String::new()
                        }
                    );
                    expr_builder.push((precedence::PATH, format!("var{}", expr_index)));
                    expr_index += 1;
                }
                I32Load8U(_log_align, offset) => {
                    let addr = expr_builder.pop_formatted(precedence::AS).unwrap();
                    println!(
                        "{}let var{} = self.mem.load8({} as usize{}) as i32;",
                        indentation,
                        expr_index,
                        addr,
                        if offset != 0 {
                            format!(" + {}", offset)
                        } else {
                            String::new()
                        }
                    );
                    expr_builder.push((precedence::PATH, format!("var{}", expr_index)));
                    expr_index += 1;
                }
                I32Load16S(_log_align, offset) => {
                    let addr = expr_builder.pop_formatted(precedence::AS).unwrap();
                    println!(
                        "{}let var{} = self.mem.load16({} as usize{}) as i16 as i32;",
                        indentation,
                        expr_index,
                        addr,
                        if offset != 0 {
                            format!(" + {}", offset)
                        } else {
                            String::new()
                        }
                    );
                    expr_builder.push((precedence::PATH, format!("var{}", expr_index)));
                    expr_index += 1;
                }
                I32Load16U(_log_align, offset) => {
                    let addr = expr_builder.pop_formatted(precedence::AS).unwrap();
                    println!(
                        "{}let var{} = self.mem.load16({} as usize{}) as i32;",
                        indentation,
                        expr_index,
                        addr,
                        if offset != 0 {
                            format!(" + {}", offset)
                        } else {
                            String::new()
                        }
                    );
                    expr_builder.push((precedence::PATH, format!("var{}", expr_index)));
                    expr_index += 1;
                }
                //                 I64Load8S(_log_align, offset) => {
                //                     let addr = stack.pop().unwrap();
                //                     println!(
                //                         "{}let var{} = self.mem.load8(var{} as usize + {}) as i8 as i64;",
                //                         indentation, expr_index, addr, offset
                //                     );
                //                     stack.push(expr_index);
                //                     expr_index += 1;
                //                 }
                I64Load8U(_log_align, offset) => {
                    let addr = expr_builder.pop_formatted(precedence::AS).unwrap();
                    println!(
                        "{}let var{} = self.mem.load8({} as usize{}) as i64;",
                        indentation,
                        expr_index,
                        addr,
                        if offset != 0 {
                            format!(" + {}", offset)
                        } else {
                            String::new()
                        }
                    );
                    expr_builder.push((precedence::PATH, format!("var{}", expr_index)));
                    expr_index += 1;
                }
                I64Load16S(_log_align, offset) => {
                    let addr = expr_builder.pop_formatted(precedence::AS).unwrap();
                    println!(
                        "{}let var{} = self.mem.load16({} as usize{}) as i16 as i64;",
                        indentation,
                        expr_index,
                        addr,
                        if offset != 0 {
                            format!(" + {}", offset)
                        } else {
                            String::new()
                        }
                    );
                    expr_builder.push((precedence::PATH, format!("var{}", expr_index)));
                    expr_index += 1;
                }
                //                 I64Load16U(_log_align, offset) => {
                //                     let addr = stack.pop().unwrap();
                //                     println!(
                //                         "{}let var{} = self.mem.load16(var{} as usize + {}) as i64;",
                //                         indentation, expr_index, addr, offset
                //                     );
                //                     stack.push(expr_index);
                //                     expr_index += 1;
                //                 }
                I64Load32S(_log_align, offset) => {
                    let addr = expr_builder.pop_formatted(precedence::AS).unwrap();
                    println!(
                        "{}let var{} = self.mem.load32({} as usize{}) as i32 as i64;",
                        indentation,
                        expr_index,
                        addr,
                        if offset != 0 {
                            format!(" + {}", offset)
                        } else {
                            String::new()
                        }
                    );
                    expr_builder.push((precedence::PATH, format!("var{}", expr_index)));
                    expr_index += 1;
                }
                I64Load32U(_log_align, offset) => {
                    let addr = expr_builder.pop_formatted(precedence::AS).unwrap();
                    println!(
                        "{}let var{} = self.mem.load32({} as usize{}) as i64;",
                        indentation,
                        expr_index,
                        addr,
                        if offset != 0 {
                            format!(" + {}", offset)
                        } else {
                            String::new()
                        }
                    );
                    expr_builder.push((precedence::PATH, format!("var{}", expr_index)));
                    expr_index += 1;
                }
                I32Store(_log_align, offset) => {
                    let value = expr_builder.pop_formatted(precedence::AS).unwrap();
                    let addr = expr_builder.pop_formatted(precedence::AS).unwrap();
                    println!(
                        "{}self.mem.store32({} as usize{}, {} as u32);",
                        indentation,
                        addr,
                        if offset != 0 {
                            format!(" + {}", offset)
                        } else {
                            String::new()
                        },
                        value
                    );
                }
                I64Store(_log_align, offset) => {
                    let value = expr_builder.pop_formatted(precedence::AS).unwrap();
                    let addr = expr_builder.pop_formatted(precedence::AS).unwrap();
                    println!(
                        "{}self.mem.store64({} as usize{}, {} as u64);",
                        indentation,
                        addr,
                        if offset != 0 {
                            format!(" + {}", offset)
                        } else {
                            String::new()
                        },
                        value
                    );
                }
                F32Store(_log_align, offset) => {
                    let value = expr_builder.pop_formatted(precedence::METHOD_CALL).unwrap();
                    let addr = expr_builder.pop_formatted(precedence::AS).unwrap();
                    println!(
                        "{}self.mem.store32({} as usize{}, {}.to_bits());",
                        indentation,
                        addr,
                        if offset != 0 {
                            format!(" + {}", offset)
                        } else {
                            String::new()
                        },
                        value
                    );
                }
                F64Store(_log_align, offset) => {
                    let value = expr_builder.pop_formatted(precedence::METHOD_CALL).unwrap();
                    let addr = expr_builder.pop_formatted(precedence::AS).unwrap();
                    println!(
                        "{}self.mem.store64({} as usize{}, {}.to_bits());",
                        indentation,
                        addr,
                        if offset != 0 {
                            format!(" + {}", offset)
                        } else {
                            String::new()
                        },
                        value
                    );
                }
                I32Store8(_log_align, offset) => {
                    let value = expr_builder.pop_formatted(precedence::AS).unwrap();
                    let addr = expr_builder.pop_formatted(precedence::AS).unwrap();
                    println!(
                        "{}self.mem.store8({} as usize{}, {} as u8);",
                        indentation,
                        addr,
                        if offset != 0 {
                            format!(" + {}", offset)
                        } else {
                            String::new()
                        },
                        value
                    );
                }
                I32Store16(_log_align, offset) => {
                    let value = expr_builder.pop_formatted(precedence::AS).unwrap();
                    let addr = expr_builder.pop_formatted(precedence::AS).unwrap();
                    println!(
                        "{}self.mem.store16({} as usize{}, {} as u16);",
                        indentation,
                        addr,
                        if offset != 0 {
                            format!(" + {}", offset)
                        } else {
                            String::new()
                        },
                        value
                    );
                }
                I64Store8(_log_align, offset) => {
                    let value = expr_builder.pop_formatted(precedence::AS).unwrap();
                    let addr = expr_builder.pop_formatted(precedence::AS).unwrap();
                    println!(
                        "{}self.mem.store8({} as usize{}, {} as u8);",
                        indentation,
                        addr,
                        if offset != 0 {
                            format!(" + {}", offset)
                        } else {
                            String::new()
                        },
                        value
                    );
                }
                I64Store16(_log_align, offset) => {
                    let value = expr_builder.pop_formatted(precedence::AS).unwrap();
                    let addr = expr_builder.pop_formatted(precedence::AS).unwrap();
                    println!(
                        "{}self.mem.store16({} as usize{}, {} as u16);",
                        indentation,
                        addr,
                        if offset != 0 {
                            format!(" + {}", offset)
                        } else {
                            String::new()
                        },
                        value
                    );
                }
                I64Store32(_log_align, offset) => {
                    let value = expr_builder.pop_formatted(precedence::AS).unwrap();
                    let addr = expr_builder.pop_formatted(precedence::AS).unwrap();
                    println!(
                        "{}self.mem.store32({} as usize{}, {} as u32);",
                        indentation,
                        addr,
                        if offset != 0 {
                            format!(" + {}", offset)
                        } else {
                            String::new()
                        },
                        value
                    );
                }
                //                 CurrentMemory(_) => {
                //                     println!("{}let var{} = self.mem.size();", indentation, expr_index);
                //                     stack.push(expr_index);
                //                     expr_index += 1;
                //                 }
                GrowMemory(_) => {
                    let pages = expr_builder.pop_formatted(precedence::AS).unwrap();
                    let dst = format!("var{}", expr_index);
                    println!(
                        "{}let {} = self.mem.grow({} as usize);",
                        indentation, dst, pages
                    );
                    expr_builder.push((precedence::PATH, dst));
                    expr_index += 1;
                }
                I32Const(c) => {
                    expr_builder.push((precedence::PATH, format!("{}i32", c)));
                }
                I64Const(c) => {
                    expr_builder.push((precedence::PATH, format!("{}i64", c)));
                }
                F32Const(c) => {
                    expr_builder.push((
                        precedence::FUNCTION_CALL,
                        format!("f32::from_bits({:#X})", c as u32),
                    ));
                }
                F64Const(c) => {
                    expr_builder.push((
                        precedence::FUNCTION_CALL,
                        format!("f64::from_bits({:#X})", c as u64),
                    ));
                }
                I32Eqz => {
                    expr_builder.unary_individual(precedence::COMPARISON, precedence::AS, |a| {
                        format!("({} == 0) as i32", a)
                    });
                }
                I32Eq => {
                    expr_builder.binary_individual(
                        precedence::COMPARISON,
                        precedence::COMPARISON,
                        precedence::AS,
                        |a, b| format!("({} == {}) as i32", a, b),
                    );
                }
                I32Ne => {
                    expr_builder.binary_individual(
                        precedence::COMPARISON,
                        precedence::COMPARISON,
                        precedence::AS,
                        |a, b| format!("({} != {}) as i32", a, b),
                    );
                }
                I32LtS => {
                    expr_builder.binary_individual(
                        precedence::MAX,
                        precedence::COMPARISON,
                        precedence::AS,
                        |a, b| format!("(({}) < {}) as i32", a, b),
                    );
                }
                I32LtU => {
                    expr_builder.binary(precedence::AS, |a, b| {
                        format!("(({} as u32) < {} as u32) as i32", a, b)
                    });
                }
                I32GtS => {
                    expr_builder.binary_individual(
                        precedence::COMPARISON,
                        precedence::COMPARISON,
                        precedence::AS,
                        |a, b| format!("({} > {}) as i32", a, b),
                    );
                }
                I32GtU => {
                    expr_builder.binary(precedence::AS, |a, b| {
                        format!("({} as u32 > {} as u32) as i32", a, b)
                    });
                }
                I32LeS => {
                    expr_builder.binary_individual(
                        precedence::COMPARISON,
                        precedence::COMPARISON,
                        precedence::AS,
                        |a, b| format!("({} <= {}) as i32", a, b),
                    );
                }
                I32LeU => {
                    expr_builder.binary(precedence::AS, |a, b| {
                        format!("({} as u32 <= {} as u32) as i32", a, b)
                    });
                }
                I32GeS => {
                    expr_builder.binary_individual(
                        precedence::COMPARISON,
                        precedence::COMPARISON,
                        precedence::AS,
                        |a, b| format!("({} >= {}) as i32", a, b),
                    );
                }
                I32GeU => {
                    expr_builder.binary(precedence::AS, |a, b| {
                        format!("({} as u32 >= {} as u32) as i32", a, b)
                    });
                }
                I64Eqz => {
                    expr_builder.unary_individual(precedence::COMPARISON, precedence::AS, |a| {
                        format!("({} == 0) as i32", a)
                    });
                }
                I64Eq => {
                    expr_builder.binary_individual(
                        precedence::COMPARISON,
                        precedence::COMPARISON,
                        precedence::AS,
                        |a, b| format!("({} == {}) as i32", a, b),
                    );
                }
                I64Ne => {
                    expr_builder.binary_individual(
                        precedence::COMPARISON,
                        precedence::COMPARISON,
                        precedence::AS,
                        |a, b| format!("({} != {}) as i32", a, b),
                    );
                }
                I64LtS => {
                    expr_builder.binary_individual(
                        precedence::MAX,
                        precedence::COMPARISON,
                        precedence::AS,
                        |a, b| format!("(({}) < {}) as i32", a, b),
                    );
                }
                I64LtU => {
                    expr_builder.binary(precedence::AS, |a, b| {
                        format!("(({} as u64) < {} as u64) as i32", a, b)
                    });
                }
                I64GtS => {
                    expr_builder.binary_individual(
                        precedence::COMPARISON,
                        precedence::COMPARISON,
                        precedence::AS,
                        |a, b| format!("({} > {}) as i32", a, b),
                    );
                }
                I64GtU => {
                    expr_builder.binary(precedence::AS, |a, b| {
                        format!("({} as u64 > {} as u64) as i32", a, b)
                    });
                }
                I64LeS => {
                    expr_builder.binary_individual(
                        precedence::COMPARISON,
                        precedence::COMPARISON,
                        precedence::AS,
                        |a, b| format!("({} <= {}) as i32", a, b),
                    );
                }
                I64LeU => {
                    expr_builder.binary(precedence::AS, |a, b| {
                        format!("({} as u64 <= {} as u64) as i32", a, b)
                    });
                }
                I64GeS => {
                    expr_builder.binary_individual(
                        precedence::COMPARISON,
                        precedence::COMPARISON,
                        precedence::AS,
                        |a, b| format!("({} >= {}) as i32", a, b),
                    );
                }
                I64GeU => {
                    expr_builder.binary(precedence::AS, |a, b| {
                        format!("({} as u64 >= {} as u64) as i32", a, b)
                    });
                }
                F32Eq => {
                    expr_builder.binary_individual(
                        precedence::COMPARISON,
                        precedence::COMPARISON,
                        precedence::AS,
                        |a, b| format!("({} == {}) as i32", a, b),
                    );
                }
                F32Ne => {
                    expr_builder.binary_individual(
                        precedence::COMPARISON,
                        precedence::COMPARISON,
                        precedence::AS,
                        |a, b| format!("({} != {}) as i32", a, b),
                    );
                }
                F32Lt => {
                    expr_builder.binary_individual(
                        precedence::COMPARISON,
                        precedence::COMPARISON,
                        precedence::AS,
                        |a, b| format!("(({}) < {}) as i32", a, b),
                    );
                }
                F32Gt => {
                    expr_builder.binary_individual(
                        precedence::COMPARISON,
                        precedence::COMPARISON,
                        precedence::AS,
                        |a, b| format!("({} > {}) as i32", a, b),
                    );
                }
                F32Le => {
                    expr_builder.binary_individual(
                        precedence::COMPARISON,
                        precedence::COMPARISON,
                        precedence::AS,
                        |a, b| format!("({} <= {}) as i32", a, b),
                    );
                }
                F32Ge => {
                    expr_builder.binary_individual(
                        precedence::COMPARISON,
                        precedence::COMPARISON,
                        precedence::AS,
                        |a, b| format!("({} >= {}) as i32", a, b),
                    );
                }
                F64Eq => {
                    expr_builder.binary_individual(
                        precedence::COMPARISON,
                        precedence::COMPARISON,
                        precedence::AS,
                        |a, b| format!("({} == {}) as i32", a, b),
                    );
                }
                F64Ne => {
                    expr_builder.binary_individual(
                        precedence::COMPARISON,
                        precedence::COMPARISON,
                        precedence::AS,
                        |a, b| format!("({} != {}) as i32", a, b),
                    );
                }
                F64Lt => {
                    expr_builder.binary_individual(
                        precedence::COMPARISON,
                        precedence::COMPARISON,
                        precedence::AS,
                        |a, b| format!("(({}) < {}) as i32", a, b),
                    );
                }
                F64Gt => {
                    expr_builder.binary_individual(
                        precedence::COMPARISON,
                        precedence::COMPARISON,
                        precedence::AS,
                        |a, b| format!("({} > {}) as i32", a, b),
                    );
                }
                F64Le => {
                    expr_builder.binary_individual(
                        precedence::COMPARISON,
                        precedence::COMPARISON,
                        precedence::AS,
                        |a, b| format!("({} <= {}) as i32", a, b),
                    );
                }
                F64Ge => {
                    expr_builder.binary_individual(
                        precedence::COMPARISON,
                        precedence::COMPARISON,
                        precedence::AS,
                        |a, b| format!("({} >= {}) as i32", a, b),
                    );
                }
                I32Clz => {
                    expr_builder.unary_individual(precedence::METHOD_CALL, precedence::AS, |a| {
                        format!("{}.leading_zeros() as i32", a)
                    });
                }
                I32Ctz => {
                    expr_builder.unary_individual(precedence::METHOD_CALL, precedence::AS, |a| {
                        format!("{}.trailing_zeros() as i32", a)
                    });
                }
                //                 I32Popcnt => {
                //                     let a = stack.pop().unwrap();
                //                     println!(
                //                         "{}let var{} = var{}.count_ones() as i32;",
                //                         indentation, expr_index, a
                //                     );
                //                     stack.push(expr_index);
                //                     expr_index += 1;
                //                 }
                I32Add => {
                    expr_builder.method_call_one_arg(precedence::METHOD_CALL, |a, b| {
                        format!("{}.wrapping_add({})", a, b)
                    });
                }
                I32Sub => {
                    expr_builder.method_call_one_arg(precedence::METHOD_CALL, |a, b| {
                        format!("{}.wrapping_sub({})", a, b)
                    });
                }
                I32Mul => {
                    expr_builder.method_call_one_arg(precedence::METHOD_CALL, |a, b| {
                        format!("{}.wrapping_mul({})", a, b)
                    });
                }
                I32DivS => {
                    expr_builder.binary_lr(precedence::DIV, |a, b| format!("{} / {}", a, b));
                }
                I32DivU => {
                    expr_builder.binary(precedence::AS, |a, b| {
                        format!("({} as u32 / {} as u32) as i32", a, b)
                    });
                }
                I32RemS => {
                    expr_builder.method_call_one_arg(precedence::METHOD_CALL, |a, b| {
                        format!("{}.wrapping_rem({})", a, b)
                    });
                }
                I32RemU => {
                    expr_builder.binary(precedence::AS, |a, b| {
                        format!("({} as u32).wrapping_rem({} as u32) as i32", a, b)
                    });
                }
                I32And => {
                    expr_builder.binary(precedence::BIT_AND, |a, b| format!("{} & {}", a, b));
                }
                I32Or => {
                    expr_builder.binary(precedence::BIT_OR, |a, b| format!("{} | {}", a, b));
                }
                I32Xor => {
                    expr_builder.binary(precedence::BIT_XOR, |a, b| format!("{} ^ {}", a, b));
                }
                I32Shl => {
                    expr_builder.binary_individual(
                        precedence::METHOD_CALL,
                        precedence::AS,
                        precedence::METHOD_CALL,
                        |a, b| format!("{}.wrapping_shl({} as u32)", a, b),
                    );
                }
                I32ShrS => {
                    expr_builder.binary_individual(
                        precedence::METHOD_CALL,
                        precedence::AS,
                        precedence::METHOD_CALL,
                        |a, b| format!("{}.wrapping_shr({} as u32)", a, b),
                    );
                }
                I32ShrU => {
                    expr_builder.binary(precedence::AS, |a, b| {
                        format!("({} as u32).wrapping_shr({} as u32) as i32", a, b)
                    });
                }
                I32Rotl => {
                    expr_builder.binary_individual(
                        precedence::METHOD_CALL,
                        precedence::AS,
                        precedence::METHOD_CALL,
                        |a, b| format!("{}.rotate_left({} as u32)", a, b),
                    );
                }
                //                 I32Rotr => {
                //                     let a = stack.pop().unwrap();
                //                     let b = stack.pop().unwrap();
                //                     println!(
                //                         "{}let var{} = var{}.rotate_right(var{} as u32);",
                //                         indentation, expr_index, b, a
                //                     );
                //                     stack.push(expr_index);
                //                     expr_index += 1;
                //                 }
                I64Clz => {
                    expr_builder.unary_individual(precedence::METHOD_CALL, precedence::AS, |a| {
                        format!("{}.leading_zeros() as i64", a)
                    });
                }
                I64Ctz => {
                    expr_builder.unary_individual(precedence::METHOD_CALL, precedence::AS, |a| {
                        format!("{}.trailing_zeros() as i64", a)
                    });
                }
                //                 I64Popcnt => {
                //                     let a = stack.pop().unwrap();
                //                     println!(
                //                         "{}let var{} = var{}.count_ones() as i64;",
                //                         indentation, expr_index, a
                //                     );
                //                     stack.push(expr_index);
                //                     expr_index += 1;
                //                 }
                I64Add => {
                    expr_builder.method_call_one_arg(precedence::METHOD_CALL, |a, b| {
                        format!("{}.wrapping_add({})", a, b)
                    });
                }
                I64Sub => {
                    expr_builder.method_call_one_arg(precedence::METHOD_CALL, |a, b| {
                        format!("{}.wrapping_sub({})", a, b)
                    });
                }
                I64Mul => {
                    expr_builder.method_call_one_arg(precedence::METHOD_CALL, |a, b| {
                        format!("{}.wrapping_mul({})", a, b)
                    });
                }
                I64DivS => {
                    expr_builder.binary_lr(precedence::DIV, |a, b| format!("{} / {}", a, b));
                }
                I64DivU => {
                    expr_builder.binary(precedence::AS, |a, b| {
                        format!("({} as u64 / {} as u64) as i64", a, b)
                    });
                }
                //                 I64RemS => {
                //                     let a = stack.pop().unwrap();
                //                     let b = stack.pop().unwrap();
                //                     println!(
                //                         "{}let var{} = var{}.wrapping_rem(var{});",
                //                         indentation, expr_index, b, a
                //                     );
                //                     stack.push(expr_index);
                //                     expr_index += 1;
                //                 }
                I64RemU => {
                    expr_builder.binary(precedence::AS, |a, b| {
                        format!("({} as u64).wrapping_rem({} as u64) as i64", a, b)
                    });
                }
                I64And => {
                    expr_builder.binary(precedence::BIT_AND, |a, b| format!("{} & {}", a, b));
                }
                I64Or => {
                    expr_builder.binary(precedence::BIT_OR, |a, b| format!("{} | {}", a, b));
                }
                I64Xor => {
                    expr_builder.binary(precedence::BIT_XOR, |a, b| format!("{} ^ {}", a, b));
                }
                I64Shl => {
                    expr_builder.binary_individual(
                        precedence::METHOD_CALL,
                        precedence::AS,
                        precedence::METHOD_CALL,
                        |a, b| format!("{}.wrapping_shl({} as u32)", a, b),
                    );
                }
                //                 I64Shl => {
                //                     let a = stack.pop().unwrap();
                //                     let b = stack.pop().unwrap();
                //                     println!(
                //                         "{}let var{} = var{}.wrapping_shl(var{} as u32);",
                //                         indentation, expr_index, b, a
                //                     );
                //                     stack.push(expr_index);
                //                     expr_index += 1;
                //                 }
                I64ShrS => {
                    expr_builder.binary_individual(
                        precedence::METHOD_CALL,
                        precedence::AS,
                        precedence::METHOD_CALL,
                        |a, b| format!("{}.wrapping_shr({} as u32)", a, b),
                    );
                }
                I64ShrU => {
                    expr_builder.binary(precedence::AS, |a, b| {
                        format!("({} as u64).wrapping_shr({} as u32) as i64", a, b)
                    });
                }
                I64Rotl => {
                    expr_builder.binary_individual(
                        precedence::METHOD_CALL,
                        precedence::AS,
                        precedence::METHOD_CALL,
                        |a, b| format!("{}.rotate_left({} as u32)", a, b),
                    );
                }
                //                 I64Rotr => {
                //                     let a = stack.pop().unwrap();
                //                     let b = stack.pop().unwrap();
                //                     println!(
                //                         "{}let var{} = var{}.rotate_right(var{} as u32);",
                //                         indentation, expr_index, b, a
                //                     );
                //                     stack.push(expr_index);
                //                     expr_index += 1;
                //                 }
                F32Abs => {
                    expr_builder.unary_individual(
                        precedence::METHOD_CALL,
                        precedence::FUNCTION_CALL,
                        |a| format!("f32::from_bits({}.to_bits() & 0x7FFF_FFFF)", a),
                    );
                }
                F32Neg => {
                    expr_builder.unary_individual(
                        precedence::METHOD_CALL,
                        precedence::FUNCTION_CALL,
                        |a| format!("f32::from_bits({}.to_bits() ^ 0x8000_0000)", a),
                    );
                }
                F32Ceil => {
                    expr_builder.unary(precedence::METHOD_CALL, |a| format!("{}.ceil()", a));
                }
                F32Floor => {
                    expr_builder.unary(precedence::METHOD_CALL, |a| format!("{}.floor()", a));
                }
                //                 F32Trunc => {
                //                     let a = stack.pop().unwrap();
                //                     println!("{}let var{} = var{}.trunc();", indentation, expr_index, a);
                //                     stack.push(expr_index);
                //                     expr_index += 1;
                //                 }
                F32Nearest => {
                    let (_, val) = expr_builder.pop().unwrap();
                    println!(
                        "{0}let var{1} = {{
{0}    let val = {2};
{0}    let round = val.round();
{0}    if val.fract().abs() != 0.5 {{
{0}        round
{0}    }} else if round % 2.0 == 1.0 {{
{0}        val.floor()
{0}    }} else if round % 2.0 == -1.0 {{
{0}        val.ceil()
{0}    }} else {{
{0}        round
{0}    }}
{0}}};",
                        indentation, expr_index, val
                    );
                    expr_builder.push((precedence::PATH, format!("var{}", expr_index)));
                    expr_index += 1;
                }
                //                 F32Sqrt => {
                //                     let a = stack.pop().unwrap();
                //                     println!("{}let var{} = var{}.sqrt();", indentation, expr_index, a);
                //                     stack.push(expr_index);
                //                     expr_index += 1;
                //                 }
                F32Add => {
                    expr_builder.binary_lr(precedence::ADD, |a, b| format!("{} + {}", a, b));
                }
                F32Sub => {
                    expr_builder.binary_lr(precedence::SUB, |a, b| format!("{} - {}", a, b));
                }
                F32Mul => {
                    expr_builder.binary_lr(precedence::MUL, |a, b| format!("{} * {}", a, b));
                }
                F32Div => {
                    expr_builder.binary_lr(precedence::DIV, |a, b| format!("{} / {}", a, b));
                }
                F32Min => {
                    let (_, b) = expr_builder.pop().unwrap();
                    let (_, a) = expr_builder.pop().unwrap();
                    expr_builder.push((
                        precedence::PATH,
                        format!("({{ let a = {}; let b = {}; if a.is_nan() || b.is_nan() {{ a }} else {{ a.min(b) }} }})", a, b),
                    ));
                }
                //                 F32Max => {
                //                     let a = stack.pop().unwrap();
                //                     let b = stack.pop().unwrap();
                //                     println!(
                //                         "{0}let var{1} = if var{2}.is_nan() || var{3}.is_nan() {{ var{2} }} else {{ var{2}.max(var{3}) }};",
                //                         indentation, expr_index, b, a
                //                     );
                //                     stack.push(expr_index);
                //                     expr_index += 1;
                //                 }
                F32Copysign => {
                    expr_builder.binary_individual(
                        precedence::METHOD_CALL,
                        precedence::METHOD_CALL,
                        precedence::FUNCTION_CALL,
                        |a, b| {
                            format!("f32::from_bits(({}.to_bits() & !(1 << 31)) | ({}.to_bits() & (1 << 31)))", a, b)
                        },
                    );
                }
                F64Abs => {
                    expr_builder.unary_individual(
                        precedence::METHOD_CALL,
                        precedence::FUNCTION_CALL,
                        |a| format!("f64::from_bits({}.to_bits() & 0x7FFF_FFFF_FFFF_FFFF)", a),
                    );
                }
                F64Neg => {
                    expr_builder.unary_individual(
                        precedence::METHOD_CALL,
                        precedence::FUNCTION_CALL,
                        |a| format!("f64::from_bits({}.to_bits() ^ 0x8000_0000_0000_0000)", a),
                    );
                }
                F64Ceil => {
                    expr_builder.unary(precedence::METHOD_CALL, |a| format!("{}.ceil()", a));
                }
                F64Floor => {
                    expr_builder.unary(precedence::METHOD_CALL, |a| format!("{}.floor()", a));
                }
                //                 F64Trunc => {
                //                     let a = stack.pop().unwrap();
                //                     println!("{}let var{} = var{}.trunc();", indentation, expr_index, a);
                //                     stack.push(expr_index);
                //                     expr_index += 1;
                //                 }
                F64Nearest => {
                    let (_, val) = expr_builder.pop().unwrap();
                    println!(
                        "{0}let var{1} = {{
{0}    let val = {2};
{0}    let round = val.round();
{0}    if val.fract().abs() != 0.5 {{
{0}        round
{0}    }} else if round % 2.0 == 1.0 {{
{0}        val.floor()
{0}    }} else if round % 2.0 == -1.0 {{
{0}        val.ceil()
{0}    }} else {{
{0}        round
{0}    }}
{0}}};",
                        indentation, expr_index, val
                    );
                    expr_builder.push((precedence::PATH, format!("var{}", expr_index)));
                    expr_index += 1;
                }
                F64Sqrt => {
                    expr_builder.unary(precedence::METHOD_CALL, |a| format!("{}.sqrt()", a));
                }
                F64Add => {
                    expr_builder.binary_lr(precedence::ADD, |a, b| format!("{} + {}", a, b));
                }
                F64Sub => {
                    expr_builder.binary_lr(precedence::SUB, |a, b| format!("{} - {}", a, b));
                }
                F64Mul => {
                    expr_builder.binary_lr(precedence::MUL, |a, b| format!("{} * {}", a, b));
                }
                F64Div => {
                    expr_builder.binary_lr(precedence::DIV, |a, b| format!("{} / {}", a, b));
                }
                //                 F64Min => {
                //                     let a = stack.pop().unwrap();
                //                     let b = stack.pop().unwrap();
                //                     println!(
                //                         "{0}let var{1} = if var{2}.is_nan() || var{3}.is_nan() {{ var{2} }} else {{ var{2}.min(var{3}) }};",
                //                         indentation, expr_index, b, a
                //                     );
                //                     stack.push(expr_index);
                //                     expr_index += 1;
                //                 }
                //                 F64Max => {
                //                     let a = stack.pop().unwrap();
                //                     let b = stack.pop().unwrap();
                //                     println!(
                //                         "{0}let var{1} = if var{2}.is_nan() || var{3}.is_nan() {{ var{2} }} else {{ var{2}.max(var{3}) }};",
                //                         indentation, expr_index, b, a
                //                     );
                //                     stack.push(expr_index);
                //                     expr_index += 1;
                //                 }
                F64Copysign => {
                    expr_builder.binary_individual(
                        precedence::METHOD_CALL,
                        precedence::METHOD_CALL,
                        precedence::FUNCTION_CALL,
                        |a, b| {
                            format!("f64::from_bits(({}.to_bits() & !(1 << 63)) | ({}.to_bits() & (1 << 63)))", a, b)
                        },
                    );
                }
                I32WrapI64 => {
                    expr_builder.unary(precedence::AS, |a| format!("{} as i32", a));
                }
                I32TruncSF32 => {
                    expr_builder.unary(precedence::AS, |a| format!("{} as i32", a));
                }
                //                 I32TruncUF32 => {
                //                     let val = stack.pop().unwrap();
                //                     println!(
                //                         "{}let var{} = var{} as u32 as i32;",
                //                         indentation, expr_index, val
                //                     );
                //                     stack.push(expr_index);
                //                     expr_index += 1;
                //                 }
                I32TruncSF64 => {
                    expr_builder.unary(precedence::AS, |a| format!("{} as i32", a));
                }
                I32TruncUF64 => {
                    expr_builder.unary(precedence::AS, |a| format!("{} as u32 as i32", a));
                }
                I64ExtendSI32 => {
                    expr_builder.unary(precedence::AS, |a| format!("{} as i64", a));
                }
                I64ExtendUI32 => {
                    expr_builder.unary(precedence::AS, |a| format!("{} as u32 as i64", a));
                }
                //                 I64TruncSF32 => {
                //                     let val = stack.pop().unwrap();
                //                     println!("{}let var{} = var{} as i64;", indentation, expr_index, val);
                //                     stack.push(expr_index);
                //                     expr_index += 1;
                //                 }
                I64TruncUF32 => {
                    expr_builder.unary(precedence::AS, |a| format!("{} as u64 as i64", a));
                }
                I64TruncSF64 => {
                    expr_builder.unary(precedence::AS, |a| format!("{} as i64", a));
                }
                I64TruncUF64 => {
                    expr_builder.unary(precedence::AS, |a| format!("{} as u64 as i64", a));
                }
                F32ConvertSI32 => {
                    expr_builder.unary(precedence::AS, |a| format!("{} as f32", a));
                }
                //                 F32ConvertUI32 => {
                //                     let val = stack.pop().unwrap();
                //                     println!(
                //                         "{}let var{} = var{} as u32 as f32;",
                //                         indentation, expr_index, val
                //                     );
                //                     stack.push(expr_index);
                //                     expr_index += 1;
                //                 }
                F32ConvertSI64 => {
                    expr_builder.unary(precedence::AS, |a| format!("{} as f32", a));
                }
                F32ConvertUI64 => {
                    expr_builder.unary(precedence::AS, |a| format!("{} as u64 as f32", a));
                }
                F32DemoteF64 => {
                    expr_builder.unary(precedence::AS, |a| format!("{} as f32", a));
                }
                F64ConvertSI32 => {
                    expr_builder.unary(precedence::AS, |a| format!("{} as f64", a));
                }
                F64ConvertUI32 => {
                    expr_builder.unary(precedence::AS, |a| format!("{} as u32 as f64", a));
                }
                F64ConvertSI64 => {
                    expr_builder.unary(precedence::AS, |a| format!("{} as f64", a));
                }
                F64ConvertUI64 => {
                    expr_builder.unary(precedence::AS, |a| format!("{} as u64 as f64", a));
                }
                F64PromoteF32 => {
                    expr_builder.unary(precedence::AS, |a| format!("{} as f64", a));
                }
                I32ReinterpretF32 => {
                    expr_builder.unary_individual(precedence::METHOD_CALL, precedence::AS, |a| {
                        format!("{}.to_bits() as i32", a)
                    });
                }
                I64ReinterpretF64 => {
                    expr_builder.unary_individual(precedence::METHOD_CALL, precedence::AS, |a| {
                        format!("{}.to_bits() as i64", a)
                    });
                }
                //                 F32ReinterpretI32 => {
                //                     let val = stack.pop().unwrap();
                //                     println!(
                //                         "{}let var{} = f32::from_bits(var{} as u32);",
                //                         indentation, expr_index, val
                //                     );
                //                     stack.push(expr_index);
                //                     expr_index += 1;
                //                 }
                F64ReinterpretI64 => {
                    expr_builder.unary_individual(precedence::AS, precedence::FUNCTION_CALL, |a| {
                        format!("f64::from_bits({} as u64)", a)
                    });
                }
                ref e => {
                    println!(
                        "{}// Unhandled {:?} (Stack: {:?})",
                        indentation, e, expr_builder
                    );
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
                let (ref name, fn_type, _, real_name) = functions[fn_index as usize];
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
                if let Some(real_name) = real_name {
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
