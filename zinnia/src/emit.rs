use calyx_ir as ir;
use calyx_ir::{structure};
use calyx_frontend as frontend;
use chumsky::primitive::group;
use chumsky::span::SimpleSpan;
use std::fmt::Binary;
use std::result;
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
        Expr::Value(val) => memory_gen_value(val.clone(), builder, binding_map),
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
            memory_gen(rem_expr, builder, binding_map)
        },
        Expr::Binary(rem_expr_1, operator, rem_expr_2) => {
            let operand1 = match &rem_expr_1.as_ref().0 {
                Expr::Id(var_name) => binding_map.get(var_name).unwrap(),
                _ => panic!("Cannot compile inlined expressions right now, use a let binding and create the var first")
            };
            let operand2 = match &rem_expr_2.as_ref().0 {
                Expr::Id(var_name) => binding_map.get(var_name).unwrap(),
                _ => panic!("Cannot compile inlined expressions right now, use a let binding and create the var first")
            };
            match operator {
                // TODO: Schedule load/adds/writeout
                BinaryOp::Add => {
                    let size = operand1.borrow().get_parameter("WIDTH").unwrap();
                    let adder_prim = builder.add_primitive("adder", "std_add", &[size]);
                    let adder_res = builder.add_primitive("res_adder", "std_reg", &[size]);
                    memory_gen_assignment(builder, None, size, &adder_res, Some(&adder_prim));
                    build_wire_assignments_comb(builder, &adder_prim, &operand1, &operand2, "adder_action");
                    Some(adder_res)
                },
                _ => {
                    println!("Unimplemented binop");
                    None
                }
            }
        }
        _ => None

    }
}

// note to self: 'sign' bool in NType refers to the int type (either i64 or u64), 'signed' bool on the NumI refers to whether it should be negative
fn memory_gen_value(prim_val: Value, builder: &mut ir::Builder, binding_map: &mut HashMap<String, Rc<RefCell<ir::Cell>>>) -> Option<Rc<RefCell<ir::Cell>>> {
    match prim_val {
        Value::Prim(p) => memory_gen_prim(p, builder, binding_map),
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
fn memory_gen_prim(prim_val: Prim, builder: &mut ir::Builder, binding_map: &mut HashMap<String, Rc<RefCell<ir::Cell>>>) -> Option<Rc<RefCell<ir::Cell>>> {
    match prim_val {
        Prim::NumI(size_opt, signed, int_val) => {
            match size_opt {
                Some(size) => {
                    // TODO: Figure out how to handle signed ints since constant! takes u64s
                    if signed {
                        let new_reg = builder.add_primitive("reg", "std_reg", &[size.width as u64]);
                        Some(new_reg)
                    } else {
                        let new_reg = builder.add_primitive("reg", "std_reg", &[size.width as u64]);
                        memory_gen_assignment(builder, Some(int_val), size.width as u64, &new_reg, None);
                        Some(new_reg)
                    }
                },
                None => {
                    // TODO: Figure out how to handle signed ints since constant! takes u64s
                    if signed {
                        let new_reg = builder.add_primitive("reg", "std_reg", &[8]);
                        Some(new_reg)
                    } else {
                        let new_reg = builder.add_primitive("reg", "std_reg", &[8]);
                        memory_gen_assignment(builder, Some(int_val), 8u64, &new_reg, None);
                        Some(new_reg)
                    }
                }
            }

        },
        Prim::Bool(_b_val) => Some(builder.add_primitive("reg", "std_reg", &[1])),
        Prim::NumF(_f_val) => Some(builder.add_primitive("reg", "std_reg", &[8])) // Warning: floats are not preserved in compilation
    }
}

fn memory_gen_assignment(builder: &mut ir::Builder, int_val: Option<u64>, size: u64, new_reg: &Rc<RefCell<ir::Cell>>, src_reg_opt: Option<&Rc<RefCell<ir::Cell>>>) {
    // Constant signal
    if int_val.is_none() {
        assert!(src_reg_opt.is_some());
    } else {
        assert!(src_reg_opt.is_none());
    }
    let mut assignment_label : String = new_reg.borrow().name().to_string();
    match src_reg_opt {
        Some(src_reg_raw) => {
            structure!(builder;
                let signal_on = constant(1, 1);
            );
            let value_load = src_reg_raw;
            assignment_label.push_str("_reg_assn");
            build_wire_assignments(builder, new_reg, &signal_on, value_load, &assignment_label);
        },
        None => {
            structure!(builder;
                let signal_on = constant(1, 1);
                let value_load = constant(int_val.unwrap(), size);
            );
            assignment_label.push_str("_load");
            build_wire_assignments(builder, new_reg, &signal_on, &value_load, &assignment_label);
        }
    };
}

fn build_wire_assignments(builder: &mut ir::Builder, new_reg: &Rc<RefCell<ir::Cell>>, signal_on: &Rc<RefCell<ir::Cell>>, value_load:&Rc<RefCell<ir::Cell>>, group_label: &str) {
    let new_group = builder.add_group(group_label);
    let write_en_assn = builder.build_assignment(
        new_reg.borrow().get("write_en"),
        signal_on.borrow().get("out"),
        ir::Guard::True,
    );
    let value_load = builder.build_assignment(
        new_reg.borrow().get("in"),
        value_load.borrow().get("out"),
        ir::Guard::True,
    );
    let done_signal = builder.build_assignment(
        new_group.borrow().get("done"),
        new_reg.borrow().get("done"),
        ir::Guard::True
    );
    let mut mut_new_group = new_group.borrow_mut();
    mut_new_group.assignments.push(write_en_assn);
    mut_new_group.assignments.push(value_load);
    mut_new_group.assignments.push(done_signal);
}

fn build_wire_assignments_comb(builder: &mut ir::Builder, new_reg: &Rc<RefCell<ir::Cell>>, left_value_load:&Rc<RefCell<ir::Cell>>, right_value_load: &Rc<RefCell<ir::Cell>>, group_label: &str) {
    let new_group = builder.add_comb_group(group_label);
    let left_value_load_assn = builder.build_assignment(
        new_reg.borrow().get("left"),
        left_value_load.borrow().get("out"),
        ir::Guard::True,
    );
    let right_value_load_assn = builder.build_assignment(
        new_reg.borrow().get("right"),
        right_value_load.borrow().get("out"),
        ir::Guard::True,
    );
    let mut mut_new_group = new_group.borrow_mut();
    mut_new_group.assignments.push(left_value_load_assn);
    mut_new_group.assignments.push(right_value_load_assn);

}
