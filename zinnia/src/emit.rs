use calyx_ir as ir;
use calyx_frontend as frontend;
use chumsky::span::SimpleSpan;
use std::{cell::RefCell, collections::HashMap, rc::Rc, thread::scope};
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
    let mut binding_map : HashMap<String, Rc<RefCell<ir::Cell>>> = HashMap::new();
    match ast {
        Some(success_parsed) => {
            memory_gen(&success_parsed.0, &mut calyx_builder, &mut binding_map);
            return Ok(ctx)
        },
        None => panic!("Lexer/parser error")
    };

}

fn memory_gen(ast: &(Expr, SimpleSpan), builder: &mut ir::Builder, binding_map: &mut HashMap<String, Rc<RefCell<ir::Cell>>>) -> Option<Rc<RefCell<ir::Cell>>> {
    let (parsed_expr, _) = ast;
    match parsed_expr {
        Expr::Value(val) => memory_gen_value(val.clone(), builder),
        Expr::Let(binding_lst, rem_expr) => {
            for binding_obj in binding_lst.iter() {
                if !binding_map.contains_key(&binding_obj.id) {
                    /* Compile the terms in the let scope */
                    let result_cell = memory_gen(binding_obj.expr.as_ref(), builder, binding_map);
                    binding_map.insert(binding_obj.id.clone(), result_cell.unwrap());
                } else {
                    panic!("ID in let binding has been reused");
                }
            }
            /* Compile terms outside of the let scope */
            dbg!(&binding_map);
            memory_gen(rem_expr, builder, binding_map)
        },
        _ => None

    }
}

// note to self: 'sign' bool in NType refers to the int type (either i64 or u64), 'signed' bool on the NumI refers to whether it should be negative
fn memory_gen_value(prim_val: Value, builder: &mut ir::Builder) -> Option<Rc<RefCell<ir::Cell>>> {
    match prim_val {
        Value::Prim(p) => memory_gen_prim(p, builder),
        Value::Vec(lst) => memory_gen_vector(lst.as_ref(), builder)
    }
}

fn memory_gen_vector(lst: &[Spanned<Prim>], builder: &mut ir::Builder) -> Option<Rc<RefCell<ir::Cell>>> {
    let mut data_vals: Vec<i64> = vec![];
    let mut max_width: u8 = 0;
    let mut is_signed_number = false;
    for (exp, _) in lst.iter() {
                match exp {
                    Prim::NumI(size_opt, signed_val, unsigned_int_val) => {
                            let n_size = size_opt.as_ref().unwrap_or(&NType {sign: false, width: 0});
                            if n_size.sign && !is_signed_number {
                                is_signed_number = true;
                            }
                            if n_size.width > max_width {
                                max_width = n_size.width;
                            }
                            let int_val = *unsigned_int_val as i64;
                            if *signed_val {
                                data_vals.push(-int_val);
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
    let vec_component = builder.add_primitive("mem", "comb_mem_d1", &[max_width as u64, lst.len() as u64, max_width.ilog2() as u64]);
    vec_component.borrow_mut().add_attribute(ir::BoolAttr::External, 1);
    return Some(vec_component);
}

// TODO: generate assignments after register allocation
fn memory_gen_prim(prim_val: Prim, builder: &mut ir::Builder) -> Option<Rc<RefCell<ir::Cell>>> {
    match prim_val {
        Prim::NumI(size_opt, signed, _int_val) => {
            match size_opt {
                Some(size) => {
                    if signed {
                        Some(builder.add_primitive("reg", "std_reg", &[size.width as u64]))
                    } else {
                        Some(builder.add_primitive("reg", "std_reg", &[size.width as u64]))
                    }
                },
                None => {
                    if signed {
                        Some(builder.add_primitive("reg", "std_reg", &[8]))
                    } else {
                        Some(builder.add_primitive("reg", "std_reg", &[8]))
                    }
                }
            }

        },
        Prim::Bool(_b_val) => Some(builder.add_primitive("reg", "std_reg", &[1])),
        Prim::NumF(_f_val) => Some(builder.add_primitive("reg", "std_reg", &[8])) // Warning: floats are not preserved in compilation
    }
}
