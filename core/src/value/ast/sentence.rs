use std::collections::HashMap;
use crate::value::SMTLibTranslatable;
use super::*;

// ------------- COMPOUND VALUES --------------
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Sentence {
    BinOp {
        a: Val<Sentence>,
        b: Val<Sentence>,
        op: BinOp,
    },
    UnaryOp {
        a: Val<Sentence>,
        op: UnaryOp,
    },
    TernaryOp {
        a: Val<Sentence>,
        b: Val<Sentence>,
        c: Val<Sentence>,
        op: TernaryOp,
    },
    Basic(Value),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TernaryOp {
    Ite,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BinOp {
    Plus,
    Minus,
    Div,
    Mul,
    Mod,
    // Comparison
    Eq,
    Neq,
    Lt,
    Lte,
    Gt,
    Gte,
    // Binary Ops
    BitOr,
    BitAnd,
    BitXor,
    LShift,
    RShift,
    // Logical Ops
    And,
    Or
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum UnaryOp {
    Not,
    BitNot,
}

impl SMTLibTranslatable for Sentence {
    fn to_smt2(&self) -> (String, Option<HashMap<String, String>>) {
        match self {
            Sentence::Basic(v) => {
                match v {
                    Value::Concrete(_) => {
                        let int_val: String = v.as_concrete()
                                            .and_then(|v| v.as_number())
                                            .and_then(|v| v.as_u64())
                                            .unwrap().to_string();
                        // Translating an unsigned int to a bitvector (of size 64)
                        let bv_val = format!("(_ bv{} 64)", int_val);
                        (bv_val, None)
                    },
                    Value::Symbolic(s) => {
                        match s {
                            SSimpleVal::SymbolicBool(symb) => {
                                let symbol = symb.0.clone();
                                (symbol.clone(), Some(HashMap::from([(symbol, "Bool".to_string())])))
                            },
                            SSimpleVal::SymbolicNumber(sn) => {
                                let symbol = sn.0.0.clone();
                                let sort = "(_ BitVec 64)".to_string();
                                // TODO(baolean): a number can probably be an Int or a BV of different sizes
                                (symbol.clone(), Some(HashMap::from([(symbol, sort)])))
                            },
                            SSimpleVal::SymbolicVector(sv) => {
                                let symbol = sv.0.0.clone();
                                /* TODO(baolean): this can also be an Array; it's probably an Array of BitVectors
                                    We should infer the type of elements and the size of the vector
                                   Also, arrays in Z3 are used to model unbounded or very large arrays.
                                   Arrays should not be used to model small finite collections of values
                                */
                                (symbol.clone(), Some(HashMap::from([(symbol, "IntVector".to_string())])))
                            }
                        }
                    }
                }
            },
            Sentence::UnaryOp { a, op } => {
                match op {
                    UnaryOp::Not => {
                        // E.g., (not (false))
                        let (expr, symbols) = a.inner().to_smt2();
                        let smtlib = format!("(not ({})", expr);
                        (smtlib, symbols)
                    },
                    UnaryOp::BitNot => {
                        let (expr, symbols) = a.inner().to_smt2();
                        let smtlib = format!("(bvnot ({})", expr);
                        (smtlib, symbols)
                    }
                }
            },
            Self::BinOp { a, b, op } => {
                let operator = match op {
                    BinOp::Eq => "=",
                    BinOp::Gt => "bvugt", // signed is ">"
                    BinOp::Gte => "bvuge", // signed is ">="
                    BinOp::Lt => "bvult", // signed is "<"
                    BinOp::Lte => "bvule", // signed is "<="
                    BinOp::Minus => "bvsub", // operator for Ints is "-"
                    BinOp::Plus => "bvadd", // operator for Ints is "+"
                    // TODO(baolean): check if those can be used with BV
                    BinOp::Mod => "mod", 
                    BinOp::Mul => "mul",
                    BinOp::And => "and",
                    BinOp::Or => "or",
                    _ => "", // TODO(baolean): complete the set of operators
                    // The following bitwise operators can only be applied to bitvectors
                    // BinOp::BitAnd => "bvand",
                    // BinOp::BitOr => "bvor",
                    // BinOp::BitXor => "bvxor",
                };

                let (expr_1, symbols_1) = a.inner().to_smt2();
                let (expr_2, symbols_2) = b.inner().to_smt2();

                let mut decl_symbols = match symbols_1 {
                    Some(_) => symbols_1.unwrap(),
                    None => HashMap::new(),
                };

                decl_symbols = match symbols_2 {
                    Some(_) => {
                        decl_symbols.extend(symbols_2.unwrap());
                        decl_symbols
                    },
                    None => decl_symbols,
                };

                let smtlib = format!("({} {} {})", operator, expr_1, expr_2);
                (smtlib, Some(decl_symbols))
            },
            Self::TernaryOp { a, b, c, op } => {
                // TODO(baolean)
                (String::new(), None)
            },
        }
    }
}