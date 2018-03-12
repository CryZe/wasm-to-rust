use parity_wasm::elements::{BlockType, Opcode, Type, TypeSection};
use expr_builder::ExprBuilder;
use {precedence, to_rs_type, Function, Global, Indentation};

pub fn build(
    mut expr_index: usize,
    evaluates_to_value: bool,
    import_count: usize,
    imported_globals_count: usize,
    functions: &[Function],
    globals: &[Global],
    types: &TypeSection,
    code: &[Opcode],
    base_indentation: usize,
) {
    let mut expr_builder = ExprBuilder::new();
    let mut block_types = Vec::new();
    let mut indentation = Indentation(base_indentation);
    let mut loop_count = 0;

    block_types.push((
        None,
        if evaluates_to_value {
            Some((precedence::PATH, String::new()))
        } else {
            None
        },
    ));

    for opcode in code {
        // println!("{}// stack: {:?}", indentation, expr_builder);
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
                    indentation.0 -= 1;
                    print!("{}}}", indentation);
                } else {
                    indentation.0 -= 1;
                    println!("{}}}", indentation);
                }
            }
            Br(relative_depth) => {
                let &(loop_info, ref block_type) = block_types
                    .iter()
                    .rev()
                    .nth(relative_depth as usize)
                    .unwrap();
                let (label, is_a_loop) = loop_info.unwrap();

                if let &Some((_, ref target_var)) = block_type {
                    let (_, expr) = expr_builder.pop().unwrap();
                    println!("{}{} = {};", indentation, target_var, expr);
                }

                println!(
                    "{}{} 'label{};",
                    indentation,
                    if is_a_loop { "continue" } else { "break" },
                    label
                );
            }
            BrIf(relative_depth) => {
                let expr = expr_builder.pop_formatted(precedence::COMPARISON).unwrap();
                let &(loop_info, ref block_type) = block_types
                    .iter()
                    .rev()
                    .nth(relative_depth as usize)
                    .unwrap();
                let (label, is_a_loop) = loop_info.unwrap();

                println!("{}if {} != 0 {{", indentation, expr);

                if let &Some((_, ref target_var)) = block_type {
                    let &(_, ref expr) = expr_builder.inner().last().unwrap();
                    println!("{}    {} = {};", indentation, target_var, expr);
                }

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
                // TODO Branch with value
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
                let function = &functions[fn_index as usize];
                let name = &function.name;
                let fn_type = function.ty;
                let real_name = function.real_name;
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
                    precedence::MAX,
                    format!("if {} != 0 {{ {} }} else {{ {} }}", c, a, b),
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
                if (i as usize) < imported_globals_count {
                    let dst = format!("var{}", expr_index);
                    println!("{}let {} = *self.imports.{}();", indentation, dst, name);
                    expr_index += 1;
                    expr_builder.push((precedence::PATH, dst));
                } else if global.is_mutable {
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
                if (i as usize) < imported_globals_count {
                    println!("{}*self.imports.{}() = {};", indentation, name, expr);
                } else {
                    println!("{}self.{} = {};", indentation, name, expr);
                }
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
            I64Load8S(_log_align, offset) => {
                let addr = expr_builder.pop_formatted(precedence::AS).unwrap();
                println!(
                    "{}let var{} = self.mem.load8({} as usize{}) as i8 as i64;",
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
            I64Load16U(_log_align, offset) => {
                let addr = expr_builder.pop_formatted(precedence::AS).unwrap();
                println!(
                    "{}let var{} = self.mem.load16({} as usize{}) as i64;",
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
            CurrentMemory(_) => {
                let dst = format!("var{}", expr_index);
                println!("{}let {} = self.mem.size();", indentation, dst);
                expr_index += 1;
                expr_builder.push((precedence::PATH, dst));
            }
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
                let precedence = if c < 0 {
                    precedence::UNARY
                } else {
                    precedence::PATH
                };
                expr_builder.push((precedence, format!("{}i32", c)));
            }
            I64Const(c) => {
                let precedence = if c < 0 {
                    precedence::UNARY
                } else {
                    precedence::PATH
                };
                expr_builder.push((precedence, format!("{}i64", c)));
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
            I32Popcnt => {
                expr_builder.unary_individual(precedence::METHOD_CALL, precedence::AS, |a| {
                    format!("{}.count_ones() as i32", a)
                });
            }
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
            I32Rotr => {
                expr_builder.binary_individual(
                    precedence::METHOD_CALL,
                    precedence::AS,
                    precedence::METHOD_CALL,
                    |a, b| format!("{}.rotate_right({} as u32)", a, b),
                );
            }
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
            I64Popcnt => {
                expr_builder.unary_individual(precedence::METHOD_CALL, precedence::AS, |a| {
                    format!("{}.count_ones() as i64", a)
                });
            }
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
            I64RemS => {
                expr_builder.method_call_one_arg(precedence::METHOD_CALL, |a, b| {
                    format!("{}.wrapping_rem({})", a, b)
                });
            }
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
            I64Rotr => {
                expr_builder.binary_individual(
                    precedence::METHOD_CALL,
                    precedence::AS,
                    precedence::METHOD_CALL,
                    |a, b| format!("{}.rotate_right({} as u32)", a, b),
                );
            }
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
            F32Trunc => {
                expr_builder.unary(precedence::METHOD_CALL, |a| format!("{}.trunc()", a));
            }
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
            F32Sqrt => {
                expr_builder.unary(precedence::METHOD_CALL, |a| format!("{}.sqrt()", a));
            }
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
                        precedence::MAX,
                        format!("{{ let a = {}; let b = {}; if a.is_nan() || b.is_nan() {{ a }} else {{ a.min(b) }} }}", a, b),
                    ));
            }
            F32Max => {
                let (_, b) = expr_builder.pop().unwrap();
                let (_, a) = expr_builder.pop().unwrap();
                expr_builder.push((
                        precedence::MAX,
                        format!("{{ let a = {}; let b = {}; if a.is_nan() || b.is_nan() {{ a }} else {{ a.max(b) }} }}", a, b),
                    ));
            }
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
            F64Trunc => {
                expr_builder.unary(precedence::METHOD_CALL, |a| format!("{}.trunc()", a));
            }
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
            F64Min => {
                let (_, b) = expr_builder.pop().unwrap();
                let (_, a) = expr_builder.pop().unwrap();
                expr_builder.push((
                        precedence::MAX,
                        format!("{{ let a = {}; let b = {}; if a.is_nan() || b.is_nan() {{ a }} else {{ a.min(b) }} }}", a, b),
                    ));
            }
            F64Max => {
                let (_, b) = expr_builder.pop().unwrap();
                let (_, a) = expr_builder.pop().unwrap();
                expr_builder.push((
                        precedence::MAX,
                        format!("{{ let a = {}; let b = {}; if a.is_nan() || b.is_nan() {{ a }} else {{ a.max(b) }} }}", a, b),
                    ));
            }
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
            I32TruncUF32 => {
                expr_builder.unary(precedence::AS, |a| format!("{} as u32 as i32", a));
            }
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
            I64TruncSF32 => {
                expr_builder.unary(precedence::AS, |a| format!("{} as i64", a));
            }
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
            F32ConvertUI32 => {
                expr_builder.unary(precedence::AS, |a| format!("{} as u32 as f32", a));
            }
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
            F32ReinterpretI32 => {
                expr_builder.unary_individual(precedence::AS, precedence::FUNCTION_CALL, |a| {
                    format!("f32::from_bits({} as u32)", a)
                });
            }
            F64ReinterpretI64 => {
                expr_builder.unary_individual(precedence::AS, precedence::FUNCTION_CALL, |a| {
                    format!("f64::from_bits({} as u64)", a)
                });
            }
        }
    }
}
