pub mod base_interpreter;

#[cfg(test)]
mod test {
    use super::base_interpreter::*;
    use crate::value::{ast::*, SMTLibTranslatable};
    use z3_sys::*;
    use std::ffi::{CStr, CString};
    
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

        let twelve = Sentence::Basic(Value::Concrete(CSimpleVal::Number(CNumber::U64(12_u64))));

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

        let x_greater_than_twelve = Sentence::BinOp {
            a: Val::new(symb_x.clone()),
            b: Val::new(twelve),
            op: BinOp::Gt,
        };

        let sum_greater_than_fifteen = Sentence::BinOp {
            a: Val::new(add_x_y),
            b: Val::new(add_five_ten),
            op: BinOp::Gte,
        };

        let pgm = Sentence::BinOp {
            a: Val::new(sum_greater_than_fifteen),
            b: Val::new(x_greater_than_twelve),
            op: BinOp::And,
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
                        // Each symbol used in the query needs to be declared first
                        // These correspond to initial values of symbolic variables
                        let var_decl = format!("(declare-fun {} () {}) ", symb.0, symb.1);
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

        let query = String::from("(declare-fun x () Int) (declare-fun y () Int) (assert (and (>= (+ x y) 15) (> x 12)))");
        // The order of x, y declaration is not deterministic, so this assertion might fail
        // if x is declared after y in the generated query
        assert_eq!(result, query);

        // Solving a generated smtlib query using Z3 low-level bindings
        unsafe {
            let cfg = Z3_mk_config();
            let ctx = Z3_mk_context(cfg);
    
            let solver = Z3_mk_simple_solver(ctx);
    
            // let assertion: CString = CString::new(result).unwrap();
            let assertion: CString = CString::new(query).unwrap();
    
            Z3_solver_from_string(ctx, solver, assertion.as_ptr());
    
            assert_eq!(Z3_solver_check(ctx, solver), Z3_L_TRUE);
    
            let model = Z3_solver_get_model(ctx, solver);
            let model_s = Z3_model_to_string(ctx, model);
    
            let model_str = CStr::from_ptr(model_s).to_str().unwrap();
            // "x -> 0 \n y -> 15"
            println!("{}", model_str.to_string());
    
            let model_elements = model_str.split_terminator('\n').collect::<Vec<_>>();

            assert_eq!(model_elements.len(), 2);
            assert!(model_elements.contains(&"y -> 2"));
            assert!(model_elements.contains(&"x -> 13")); // x > 12 && (x + y) >= 15 
        }
    }

}
