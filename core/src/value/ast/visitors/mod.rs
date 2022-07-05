pub mod base_interpreter;

#[cfg(test)]
mod test {
    use super::base_interpreter::*;
    use crate::value::{ast::*, SMTLibTranslatable};

    #[test]
    fn test_add_pgm() {
        let five = Sentence::Basic(Value::Concrete(CSimpleVal::Number(CNumber::U64(5_u64))));

        let ten = Sentence::Basic(Value::Concrete(CSimpleVal::Number(CNumber::U64(10_u64))));

        let twenty = Sentence::Basic(Value::Concrete(CSimpleVal::Number(CNumber::U64(20_u64))));

        let sum = Sentence::Basic(Value::Concrete(CSimpleVal::Number(CNumber::U64(40_u64))));

        let add_a_b = Sentence::BinOp {
            a: Val::new(five.clone()),
            b: Val::new(ten),
            op: BinOp::Plus,
        };
        let add_a_c = Sentence::BinOp {
            a: Val::new(five.clone()),
            b: Val::new(twenty),
            op: BinOp::Plus,
        };
        let pgm = Sentence::BinOp {
            a: Val::new(add_a_b),
            b: Val::new(add_a_c),
            op: BinOp::Plus,
        };

        // Post hook to collapse binary addition between two literals.
        // This effectively simplifies (flattens) a tree of addition operations into a single SimpleVal
        let post_hook = |s: Sentence| -> Option<Sentence> {
            if let Sentence::BinOp {
                a,
                b,
                op: BinOp::Plus,
            } = s
            {
                match (a.inner().as_ref(), b.inner().as_ref()) {
                    (Sentence::Basic(aa), Sentence::Basic(bb)) => {
                        let aa = aa
                            .clone()
                            .into_concrete()
                            .ok()
                            .and_then(|aa| aa.into_number().ok())
                            .and_then(|n| n.into_u64().ok())
                            .unwrap()
                            .clone();
                        let bb = bb
                            .clone()
                            .into_concrete()
                            .ok()
                            .and_then(|bb| bb.into_number().ok())
                            .and_then(|n| n.into_u64().ok())
                            .unwrap()
                            .clone();

                        let sum = aa + bb;
                        Some(Sentence::Basic(Value::Concrete(CSimpleVal::Number(
                            CNumber::U64(sum),
                        ))))
                    }
                    _ => None,
                }
            } else {
                None
            }
        };

        // This hook effectively transforms a final BasicVal into a u64
        let final_hook = |s: Sentence| -> u64 {
            if let Sentence::Basic(v) = s {
                let val = v
                    .as_concrete()
                    .and_then(|v| v.as_number())
                    .and_then(|v| v.as_u64())
                    .unwrap()
                    .clone();
                val
            } else {
                0
            }
        };
        let pre_hook = |s: Sentence| -> Option<Sentence> { None };

        let interpreter = Interpreter { pgm };
        let result = interpreter.interpret(Box::new(pre_hook), Box::new(post_hook), final_hook);
        assert_eq!(result, 40);
    }

    #[test]
    fn test_symb() {
        let symb_x = Sentence::Basic(Value::Symbolic(SSimpleVal::SymbolicNumber(SNumber(SymbolId('x'.to_string(), None)))));
        let symb_y = Sentence::Basic(Value::Symbolic(SSimpleVal::SymbolicNumber(SNumber(SymbolId('y'.to_string(), None)))));

        let five = Sentence::Basic(Value::Concrete(CSimpleVal::Number(CNumber::U64(5_u64))));
        let ten = Sentence::Basic(Value::Concrete(CSimpleVal::Number(CNumber::U64(10_u64))));

        let add_x_y = Sentence::BinOp {
            a: Val::new(symb_x.clone()),
            b: Val::new(symb_y),
            op: BinOp::Plus,
        };

        let add_five_ten = Sentence::BinOp {
            a: Val::new(five.clone()),
            b: Val::new(ten),
            op: BinOp::Plus,
        };

        let pgm = Sentence::BinOp {
            a: Val::new(add_x_y),
            b: Val::new(add_five_ten),
            op: BinOp::Gte,
        };

        // Post hook to collapse binary addition between two literals
        // Iff both are concrete values
        let post_hook = |s: Sentence| -> Option<Sentence> {
            if let Sentence::BinOp {
                a,
                b,
                op: BinOp::Plus,
            } = s
            {
                match (a.inner().as_ref(), b.inner().as_ref()) {
                    (Sentence::Basic(aa), Sentence::Basic(bb)) => {
                        match (aa, bb) {
                            (Value::Concrete(_), Value::Concrete(_)) => {
                                let aa = aa
                                    .clone()
                                    .into_concrete()
                                    .ok()
                                    .and_then(|aa| aa.into_number().ok())
                                    .and_then(|n| n.into_u64().ok())
                                    .unwrap()
                                    .clone();
                                let bb = bb
                                    .clone()
                                    .into_concrete()
                                    .ok()
                                    .and_then(|bb| bb.into_number().ok())
                                    .and_then(|n| n.into_u64().ok())
                                    .unwrap()
                                    .clone();
        
                                let sum = aa + bb;
                                Some(Sentence::Basic(Value::Concrete(CSimpleVal::Number(
                                    CNumber::U64(sum),
                                ))))
                            },
                            _ => None
                        }
                    },
                    _ => None
                }
            } else {
                None
            }
        };

        // Final_hook translates the resulting AST to its smtlib2 representation
        let final_hook = |s: Sentence| -> String {
            let (smtlib, symbols) = s.to_smt2();

            let mut declarations = String::new();
            
            match symbols {
                Some(_) => {
                    for symb in symbols.unwrap().drain() {
                        // TODO(baolean): other variable types
                        let var_decl = format!("(declare-fun {} () Int) ", symb);
                        declarations.push_str(var_decl.as_str())        
                    }

                },
                None => ()
            };
        
            let smtlib_query = format!("{}(assert {})", declarations, smtlib);
            smtlib_query             
        };

        let pre_hook = |s: Sentence| -> Option<Sentence> { None };

        let interpreter = Interpreter { pgm };

        let result = interpreter.interpret(Box::new(pre_hook), Box::new(post_hook), final_hook);

        let query = String::from("(declare-fun x () Int) (declare-fun y () Int) (assert (>= (+ x y) 15))");
        assert_eq!(result, query);


    }

}
