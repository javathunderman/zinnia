use calyx_ir as ir;
use calyx_frontend as frontend;
use chumsky::span::SimpleSpan;
use std::collections::HashSet;
use serde_json::json;

use crate::{ast::*, Spanned};

pub fn emit(ast: Option<((Expr, SimpleSpan), SimpleSpan)>) -> Result<ir::Context, calyx_utils::Error> {
    let mut ws = frontend::Workspace::from_compile_lib()?;
    let mut ns:frontend::NamespaceDef = frontend::NamespaceDef::construct(&Some("../../838l-build-chain/calyx/primitives/memories/comb.futil".into()))?;
    let (_, externs) = ns.externs.pop().unwrap();

    let main: frontend::ast::ComponentDef = frontend::ast::ComponentDef::new(ir::Id::new("main"), false, None, vec![]);
    ws.components.push(main);

    let mut ctx = ir::from_ast::ast_to_ir(ws)?;
    let main_component = &mut ctx.components[0];

    for ext in externs {
        ctx.lib.add_extern_primitive("../memories/comb.futil".into(), ext);
    }
    let mut calyx_builder = ir::Builder::new(main_component, &ctx.lib);

    match ast {
        Some(success_parsed) => memory_gen(success_parsed, &mut calyx_builder),
        None => println!("Lexer/parser error")
    };
    Ok(ctx)
}

fn memory_gen(ast: ((Expr, SimpleSpan), SimpleSpan), builder: &mut ir::Builder) {
    let ((parsed_expr, _), _) = ast;
    let mut binding_lst: HashSet<&str> = HashSet::new();
    match parsed_expr {
        Expr::Value(val) => memory_gen_value(val, builder),
        _ => println!("Unimplemented")
    };
}

// note to self: 'sign' bool in NType refers to the int type (either i64 or u64), 'signed' bool on the NumI refers to whether it should be negative
fn memory_gen_value(prim_val: Value, builder: &mut ir::Builder) {
    match prim_val {
        Value::Prim(p) => memory_gen_prim(p, builder),
        Value::Vec(lst) => memory_gen_vector(lst.as_ref(), builder),
    }
}

fn memory_gen_vector(lst: &[Spanned<Prim>], builder: &mut ir::Builder) {
    let mut data_vals: Vec<i64> = vec![];
    let mut max_width: u8 = 0;
    let mut is_signed_number = false;
    for (exp, _) in lst.iter() {
                match exp {
                    Prim::NumI(size_opt, signed_val, unsigned_int_val) => {
                            let n_size = size_opt.as_ref().unwrap_or_else(|| &NType {sign: false, width: 0});
                            if n_size.sign && !is_signed_number {
                                is_signed_number = true;
                            }
                            if n_size.width > max_width {
                                max_width = n_size.width;
                            }
                            let int_val = *unsigned_int_val as i64;
                            if *signed_val {
                                data_vals.push(-1 * int_val);
                            } else {
                                data_vals.push(int_val);
                            }
                        },
                    Prim::NumF(_float_val) => println!("Float element in vector compilation is unimplemented"),
                    Prim::Bool(b_val) => {
                        max_width = 1;
                        if *b_val {
                            data_vals.push(1);
                        } else {
                            data_vals.push(0);
                        }
                    }
                }
    }

    if max_width == 0 {
        max_width = 8;
    }

    let data_file = json!({
        "mem":
        {
            "data": data_vals,
            "format" : {
                "numeric_type": "bitnum",
                "is_signed" : is_signed_number,
                "width": max_width
            }
        }
    });
    dbg!(data_file.to_string());
    builder.add_primitive("fsm", "comb_mem_d1", &vec![max_width as u64, lst.len() as u64, max_width.ilog2() as u64]);
}

// TODO: generate assignments after register allocation
fn memory_gen_prim(prim_val: Prim, builder: &mut ir::Builder) {
    match prim_val {
        Prim::NumI(size_opt, signed, _int_val) => {
            match size_opt {
                Some(size) => {
                    if signed {
                        builder.add_primitive("fsm", "std_reg", &vec![size.width as u64]);
                    } else {
                        builder.add_primitive("fsm", "std_reg", &vec![size.width as u64]);
                    }
                },
                None => {
                    if signed {
                        builder.add_primitive("fsm", "std_reg", &vec![8]);
                    } else {
                        builder.add_primitive("fsm", "std_reg", &vec![8]);
                    }
                }
            }

        },
        Prim::Bool(_b_val) => {
            builder.add_primitive("fsm", "std_reg", &vec![1]);
        },
        Prim::NumF(_f_val) => println!("Float compilation as primitive is unimplemented")
    }
}
