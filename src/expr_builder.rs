use precedence;

#[derive(Debug)]
pub struct ExprBuilder {
    stack: Vec<(usize, String)>,
}

impl ExprBuilder {
    pub fn new() -> Self {
        Self { stack: Vec::new() }
    }

    pub fn unary<F>(&mut self, precedence: usize, f: F)
    where
        F: FnOnce(&Formatted) -> String,
    {
        let a = self.stack.pop().unwrap();
        let expr = f(&format(precedence, a));
        self.stack.push((precedence, expr));
    }

    pub fn unary_individual<F>(&mut self, precedence_a: usize, precedence_result: usize, f: F)
    where
        F: FnOnce(&Formatted) -> String,
    {
        let a = self.stack.pop().unwrap();
        let expr = f(&format(precedence_a, a));
        self.stack.push((precedence_result, expr));
    }

    pub fn binary<F>(&mut self, precedence: usize, f: F)
    where
        F: FnOnce(&Formatted, &Formatted) -> String,
    {
        let b = self.stack.pop().unwrap();
        let a = self.stack.pop().unwrap();
        let expr = f(&format(precedence, a), &format(precedence, b));
        self.stack.push((precedence, expr));
    }

    pub fn binary_individual<F>(
        &mut self,
        precedence_a: usize,
        precedence_b: usize,
        precedence_result: usize,
        f: F,
    ) where
        F: FnOnce(&Formatted, &Formatted) -> String,
    {
        let b = self.stack.pop().unwrap();
        let a = self.stack.pop().unwrap();
        let expr = f(&format(precedence_a, a), &format(precedence_b, b));
        self.stack.push((precedence_result, expr));
    }

    pub fn binary_lr<F>(&mut self, precedence: usize, f: F)
    where
        F: FnOnce(&Formatted, &Formatted) -> String,
    {
        let b = self.stack.pop().unwrap();
        let a = self.stack.pop().unwrap();
        let expr = f(
            &format(precedence, a),
            &format(
                if b.0 == precedence {
                    precedence::MIN
                } else {
                    precedence
                },
                b,
            ),
        );
        self.stack.push((precedence, expr));
    }

    pub fn method_call_one_arg<F>(&mut self, precedence: usize, f: F)
    where
        F: FnOnce(&Formatted, &str) -> String,
    {
        let (_, b) = self.stack.pop().unwrap();
        let a = self.stack.pop().unwrap();
        let expr = f(&format(precedence, a), &b);
        self.stack.push((precedence, expr));
    }

    pub fn push(&mut self, v: (usize, String)) {
        self.stack.push(v);
    }

    pub fn pop(&mut self) -> Option<(usize, String)> {
        self.stack.pop()
    }

    pub fn pop_formatted(&mut self, precedence: usize) -> Option<Formatted> {
        format(precedence, self.stack.pop()?).into()
    }

    pub fn len(&self) -> usize {
        self.stack.len()
    }

    pub fn inner(&mut self) -> &mut Vec<(usize, String)> {
        &mut self.stack
    }
}

fn format(outer_precedence: usize, (inner_precedence, s): (usize, String)) -> Formatted {
    Formatted {
        outer_precedence,
        inner_precedence,
        s,
    }
}

pub struct Formatted {
    outer_precedence: usize,
    inner_precedence: usize,
    s: String,
}

use std::fmt;

impl fmt::Display for Formatted {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.inner_precedence > self.outer_precedence
            || (self.inner_precedence == precedence::COMPARISON
                && self.outer_precedence == precedence::COMPARISON)
        {
            write!(f, "({})", self.s)
        } else {
            write!(f, "{}", self.s)
        }
    }
}
