use std::collections::HashSet;
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
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum UnaryOp {
    Not,
    BitNot,
}

impl SMTLibTranslatable for Sentence {
    fn to_smt2(&self) -> (String, Option<HashSet<String>>) {
        match self {
            Sentence::Basic(v) => {
                match v {
                    Value::Concrete(_) => {
                        let val: String = v.as_concrete()
                                            .and_then(|v| v.as_number())
                                            .and_then(|v| v.as_u64())
                                            .unwrap().to_string();
                        (val, None)
                    },
                    Value::Symbolic(s) => {
                        match s {
                            SSimpleVal::SymbolicBool(symb) => {
                                let symbol = symb.0.clone();
                                (symbol.clone(), Some(HashSet::from([symbol])))
                            },
                            SSimpleVal::SymbolicNumber(sn) => {
                                let symbol = sn.0.0.clone();
                                (symbol.clone(), Some(HashSet::from([symbol])))
                            },
                            SSimpleVal::SymbolicVector(sv) => {
                                // TODO(baolean)
                                let symbol = sv.0.0.clone();
                                (symbol.clone(), Some(HashSet::from([symbol])))
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
                    BinOp::Gt => ">", // signed version; unsigned is 'UGT';
                    BinOp::Gte => ">=", // unsigned is 'UGE', etc.
                    BinOp::Lt => "<",
                    BinOp::Lte => "<=",
                    BinOp::Minus => "-",
                    BinOp::Mod => "mod",
                    BinOp::Mul => "*",
                    BinOp::Plus => "+",
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
                    None => HashSet::new(),
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