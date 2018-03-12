// Based on https://doc.rust-lang.org/reference/expressions.html#expression-precedence
pub const MIN: usize = 0;
pub const PATH: usize = 0;
pub const METHOD_CALL: usize = 1;
// pub const FIELD: usize = 2;
pub const FUNCTION_CALL: usize = 3;
// pub const ARRAY_INDEXING: usize = 3;
// pub const QUESTION_MARK: usize = 4;
pub const UNARY: usize = 5;
pub const AS: usize = 6;
// pub const COLON: usize = 6;
pub const MUL: usize = 7;
pub const DIV: usize = 7;
// pub const REM: usize = 7;
pub const ADD: usize = 8;
pub const SUB: usize = 8;
// pub const SHIFT: usize = 9;
pub const BIT_AND: usize = 10;
pub const BIT_XOR: usize = 11;
pub const BIT_OR: usize = 12;
pub const COMPARISON: usize = 13;
// pub const LOGIC_AND: usize = 14;
// pub const LOGIC_OR: usize = 15;
pub const MAX: usize = 999;
