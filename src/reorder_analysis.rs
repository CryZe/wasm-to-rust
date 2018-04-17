use parity_wasm::elements::{BlockType,
                            Opcode::{self, *},
                            Type,
                            TypeSection};
use {BlockKind, Function};

pub fn can_local_be_reordered(
    local_to_load: u32,
    blocks: &[BlockKind],
    functions: &[Function],
    types: &TypeSection,
    remaining_ops: &[Opcode],
) -> bool {
    let mut position_on_stack = 0isize;
    let mut stack_frames = blocks
        .iter()
        .map(|b| match *b {
            BlockKind::Function { evaluates_to_value } => (-1, evaluates_to_value, true),
            BlockKind::Block { ref dst_var, .. } | BlockKind::If { ref dst_var, .. } => {
                (-1, dst_var.is_some(), true)
            }
            | BlockKind::Loop { ref dst_var, .. } => (-1, dst_var.is_some(), false),
        })
        .collect::<Vec<_>>();

    for opcode in remaining_ops {
        match *opcode {
            Unreachable => {}
            Nop => {}
            Block(block_type) => {
                let evaluates_to_value = if let BlockType::Value(_) = block_type {
                    true
                } else {
                    false
                };
                stack_frames.push((position_on_stack, evaluates_to_value, true));
            }
            Loop(block_type) => {
                let evaluates_to_value = if let BlockType::Value(_) = block_type {
                    true
                } else {
                    false
                };
                stack_frames.push((position_on_stack, evaluates_to_value, false));
            }
            If(block_type) => {
                position_on_stack -= 1;
                if position_on_stack < 0 {
                    return true;
                }
                let evaluates_to_value = if let BlockType::Value(_) = block_type {
                    true
                } else {
                    false
                };
                stack_frames.push((position_on_stack, evaluates_to_value, true));
            }
            Else => {
                let &(pos, _, _) = stack_frames.last().unwrap();
                position_on_stack = pos;
                if position_on_stack < 0 {
                    return true;
                }
            }
            End => {
                let (pos, evaluates_to_value, _) = stack_frames.pop().unwrap();
                position_on_stack = pos;
                if position_on_stack < 0 {
                    return true;
                }
                if evaluates_to_value {
                    position_on_stack += 1;
                }
            }
            Br(relative_depth) => {
                let last = stack_frames.len() - 1;
                let &(_, evaluates_to_value, breaks_to_value) =
                    stack_frames.get(last - relative_depth as usize).unwrap();

                if evaluates_to_value && breaks_to_value {
                    position_on_stack -= 1;
                    if position_on_stack < 0 {
                        return true;
                    }
                }
            }
            BrIf(relative_depth) => {
                position_on_stack -= 1;
                if position_on_stack < 0 {
                    return true;
                }

                let last = stack_frames.len() - 1;
                let &(_, evaluates_to_value, breaks_to_value) =
                    stack_frames.get(last - relative_depth as usize).unwrap();

                if evaluates_to_value && breaks_to_value {
                    position_on_stack -= 1;
                    if position_on_stack < 0 {
                        return true;
                    }
                }
            }
            BrTable(..) => {
                position_on_stack -= 1;
                if position_on_stack < 0 {
                    return true;
                }

                // TODO Branch with value
            }
            Return => {
                // TODO Weird logic
                if position_on_stack >= 0 {
                    position_on_stack -= 1;
                    if position_on_stack < 0 {
                        return true;
                    }
                }
            }
            Call(fn_index) => {
                let function = &functions[fn_index as usize];
                let fn_type = function.ty;

                // Params
                position_on_stack -= fn_type.params().len() as isize;
                if position_on_stack < 0 {
                    return true;
                }

                // Return type
                if fn_type.return_type().is_some() {
                    position_on_stack += 1;
                }
            }
            CallIndirect(type_index, _) => {
                let Type::Function(ref fn_type) = types.types()[type_index as usize];

                // Fn Ptr
                position_on_stack -= 1;
                if position_on_stack < 0 {
                    return true;
                }

                // Params
                position_on_stack -= fn_type.params().len() as isize;
                if position_on_stack < 0 {
                    return true;
                }

                // Return type
                if fn_type.return_type().is_some() {
                    position_on_stack += 1;
                }
            }
            Drop => {
                position_on_stack -= 1;
                if position_on_stack < 0 {
                    return true;
                }
            }
            Select => {
                position_on_stack -= 3;
                if position_on_stack < 0 {
                    return true;
                }
                position_on_stack += 1;
            }
            GetLocal(_) => {
                // TODO We could merge the loads possibly
                position_on_stack += 1;
            }
            SetLocal(i) => {
                position_on_stack -= 1;
                if position_on_stack < 0 {
                    return true;
                }

                if i == local_to_load {
                    return false;
                }
            }
            TeeLocal(i) => {
                position_on_stack -= 1;
                if position_on_stack < 0 {
                    return true;
                }

                if i == local_to_load {
                    return false;
                }

                position_on_stack += 1;
            }
            GetGlobal(_) => {
                position_on_stack += 1;
            }
            SetGlobal(_) => {
                position_on_stack -= 1;
                if position_on_stack < 0 {
                    return true;
                }
            }

            // 2 committing, 0 in, 0 out
            I32Store(..) | I64Store(..) | F32Store(..) | F64Store(..) | I32Store8(..)
            | I32Store16(..) | I64Store8(..) | I64Store16(..) | I64Store32(..) => {
                position_on_stack -= 2;
                if position_on_stack < 0 {
                    return true;
                }
            }

            // 1 committing, 0 in, 1 out
            I32Load(..) | I64Load(..) | F32Load(..) | F64Load(..) | I32Load8S(..)
            | I32Load8U(..) | I32Load16S(..) | I32Load16U(..) | I64Load8S(..) | I64Load8U(..)
            | I64Load16S(..) | I64Load16U(..) | I64Load32S(..) | I64Load32U(..)
            | GrowMemory(..) | F32Nearest | F64Nearest => {
                if position_on_stack == 0 {
                    return true;
                }
            }

            // 0 committing, 0 in, 1 out
            CurrentMemory(..) | I32Const(..) | I64Const(..) | F32Const(..) | F64Const(..) => {
                position_on_stack += 1;
            }

            // 0 committing, 1 in, 1 out
            I32Eqz | I64Eqz | I32Clz | I32Ctz | I32Popcnt | I64Clz | I64Ctz | I64Popcnt
            | F32Abs | F32Neg | F32Ceil | F32Floor | F32Trunc | F32Sqrt | F64Abs | F64Neg
            | F64Ceil | F64Floor | F64Trunc | F64Sqrt | I32WrapI64 | I32TruncSF32
            | I32TruncUF32 | I32TruncSF64 | I32TruncUF64 | I64ExtendSI32 | I64ExtendUI32
            | I64TruncSF32 | I64TruncUF32 | I64TruncSF64 | I64TruncUF64 | F32ConvertSI32
            | F32ConvertUI32 | F32ConvertSI64 | F32ConvertUI64 | F32DemoteF64 | F64ConvertSI32
            | F64ConvertUI32 | F64ConvertSI64 | F64ConvertUI64 | F64PromoteF32
            | I32ReinterpretF32 | I64ReinterpretF64 | F32ReinterpretI32 | F64ReinterpretI64 => {}

            // 0 committing, 2 in, 1 out
            I32Eq | I32Ne | I32LtS | I32LtU | I32GtS | I32GtU | I32LeS | I32LeU | I32GeS
            | I32GeU | I64Eq | I64Ne | I64LtS | I64LtU | I64GtS | I64GtU | I64LeS | I64LeU
            | I64GeS | I64GeU | F32Eq | F32Ne | F32Lt | F32Gt | F32Le | F32Ge | F64Eq | F64Ne
            | F64Lt | F64Gt | F64Le | F64Ge | I32Add | I32Sub | I32Mul | I32DivS | I32DivU
            | I32RemS | I32RemU | I32And | I32Or | I32Xor | I32Shl | I32ShrS | I32ShrU
            | I32Rotl | I32Rotr | I64Add | I64Sub | I64Mul | I64DivS | I64DivU | I64RemS
            | I64RemU | I64And | I64Or | I64Xor | I64Shl | I64ShrS | I64ShrU | I64Rotl
            | I64Rotr | F32Add | F32Sub | F32Mul | F32Div | F32Min | F32Max | F32Copysign
            | F64Add | F64Sub | F64Mul | F64Div | F64Min | F64Max | F64Copysign => {
                position_on_stack -= 1;
            }
        }
    }

    panic!("Unexpected end of code")
}
