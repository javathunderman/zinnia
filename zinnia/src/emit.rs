use calyx_frontend as frontend;
use calyx_ir as ir;
use calyx_ir::structure;
use chumsky::span::SimpleSpan;
use serde_json::json;
use std::path::{Path, PathBuf};
use std::{cell::RefCell, collections::HashMap, ops::Deref, rc::Rc};

use crate::{ast::*, Spanned};

pub fn emit(
    ast: Option<((Expr, SimpleSpan), SimpleSpan)>,
) -> Result<ir::Context, calyx_utils::Error> {

        let mut ws = frontend::Workspace::construct_with_all_deps::<false>(
        vec![
            "/home/home/src/calyx/primitives/memories/comb.futil".into(),
            "/home/home/src/calyx/primitives/core.futil".into(),
            "/home/home/src/838l-project/zinnia/src/calyx-bindings/scan.futil".into(),
            "/home/home/src/838l-project/zinnia/src/calyx-bindings/list_filter.futil".into(),
        ],
        Path::new("/home/home/src/calyx/"),
    )?;

    let main: frontend::ast::ComponentDef =
        frontend::ast::ComponentDef::new(ir::Id::new("main"), false, None, vec![]);

    ws.components.push(main);

    let mut ctx = ir::from_ast::ast_to_ir(ws)?;
    let main_component = &mut ctx.components[0];

    let mut calyx_builder = ir::Builder::new(main_component, &ctx.lib);
    let mut binding_map: HashMap<String, Rc<RefCell<ir::Cell>>> = HashMap::new();
    let mut assignment_map: HashMap<String, Rc<RefCell<ir::Group>>> = HashMap::new();
    match ast {
        Some(success_parsed) => {
            memory_gen(
                &success_parsed.0,
                &mut calyx_builder,
                &mut binding_map,
                &mut assignment_map,
            );
            return Ok(ctx);
        }
        None => panic!("Lexer/parser error"),
    };
}

fn memory_gen(
    ast: &(Expr, SimpleSpan),
    builder: &mut ir::Builder,
    binding_map: &mut HashMap<String, Rc<RefCell<ir::Cell>>>,
    assignment_map: &mut HashMap<String, Rc<RefCell<ir::Group>>>,
) -> Option<Rc<RefCell<ir::Cell>>> {
    let (parsed_expr, _) = ast;
    match parsed_expr {
        Expr::Value(val) => memory_gen_value(val.clone(), builder, assignment_map),
        Expr::Let(binding_lst, rem_expr) => {
            for binding_obj in binding_lst.iter() {
                if !binding_map.contains_key(&binding_obj.id) {
                    /* Compile the terms in the let scope */
                    let result_cell = memory_gen(
                        binding_obj.expr.as_ref(),
                        builder,
                        binding_map,
                        assignment_map,
                    );
                    binding_map.insert(binding_obj.id.clone(), result_cell.unwrap());
                } else {
                    panic!("ID in let binding has been reused");
                }
            }
            /* Compile terms outside of the let scope */
            memory_gen(rem_expr, builder, binding_map, assignment_map)
        }
        Expr::Binary(rem_expr_1, operator, rem_expr_2) => {
            let operand1 : &Rc<RefCell<ir::Cell>> = match &rem_expr_1.as_ref().0 {
                Expr::Id(var_name) => binding_map.get(var_name).unwrap(),
                _ => panic!("Cannot compile inlined expressions right now, use a let binding and create the var first")
            };
            let operand2 : &Rc<RefCell<ir::Cell>> = match &rem_expr_2.as_ref().0 {
                Expr::Id(var_name) => binding_map.get(var_name).unwrap(),
                _ => panic!("Cannot compile inlined expressions right now, use a let binding and create the var first")
            };

            let size = operand1.borrow().get_parameter("WIDTH").unwrap(); // assuming the typechecker will catch non-matching bit widths
            let mut res_size = size;
            let binop_prim = match operator {
                BinaryOp::Add => builder.add_primitive("adder", "std_add", &[size]),
                BinaryOp::Sub => builder.add_primitive("subtract", "std_sub", &[size]),
                BinaryOp::Eq => {
                    res_size = 1;
                    builder.add_primitive("equality", "std_eq", &[size])
                }
                BinaryOp::Leq => {
                    res_size = 1;
                    builder.add_primitive("leq", "std_le", &[size])
                }
                BinaryOp::Geq => {
                    res_size = 1;
                    builder.add_primitive("geq", "std_ge", &[size])
                }
                BinaryOp::Lt => {
                    res_size = 1;
                    builder.add_primitive("lt", "std_lt", &[size])
                }
                BinaryOp::Gt => {
                    res_size = 1;
                    builder.add_primitive("gt", "std_gt", &[size])
                }
                BinaryOp::NotEq => {
                    res_size = 1;
                    builder.add_primitive("neq", "std_neq", &[size])
                }
                _ => panic!("Unimplemented binop"),
            };
            build_binop_assignments(
                builder,
                binop_prim,
                operand1,
                operand2,
                assignment_map,
                res_size,
            );
            memory_gen(&rem_expr_2, builder, binding_map, assignment_map)
        }
        Expr::Call(func_expr, func_args) => match &func_expr.as_ref().0 {
            Expr::Id(func_name) => match func_name.as_str() {
                "filter" => match &func_args.0.get(0).unwrap().0 {
                    Expr::Id(vec_id) => {
                        Some(invoke_filter(&vec_id, builder, binding_map, assignment_map))
                    }
                    _ => panic!("Not a vector variable, try using a let binding"),
                },
                "scan" => match &func_args.0.get(0).unwrap().0 {
                    Expr::Id(vec_id) => {
                        Some(invoke_scan(&vec_id, builder, binding_map, assignment_map))
                    }
                    _ => panic!("Not a vector variable, try using a let binding"),
                },
                _ => panic!("Unknown function name in call"),
            },
            _ => panic!("No support for user defined functions at the moment"),
        },
        _ => None,
    }
}

fn build_wire_from_memory(
    builder: &mut ir::Builder,
    new_reg: &Rc<RefCell<ir::Cell>>,
    vec_origin: &Rc<RefCell<ir::Cell>>,
    bit_width: u64,
    vec_index: u64,
    group_label: &str,
) -> Rc<RefCell<ir::Group>> {
    let new_group = builder.add_group(group_label);
    structure!(builder;
        let signal_on = constant(1, 1);
        let read_from_addr = constant(bit_width, vec_index);
    );
    let write_en_assn = builder.build_assignment(
        new_reg.borrow().get("write_en"),
        signal_on.borrow().get("out"),
        ir::Guard::True,
    );
    let seek_addr = builder.build_assignment(
        vec_origin.borrow().get("addr0"),
        read_from_addr.borrow().get("out"),
        ir::Guard::True,
    );
    let value_load = builder.build_assignment(
        new_reg.borrow().get("in"),
        vec_origin.borrow().get("read_data"),
        ir::Guard::True,
    );
    let done_signal = builder.build_assignment(
        new_group.borrow().get("done"),
        new_reg.borrow().get("done"),
        ir::Guard::True,
    );
    let mut mut_new_group = new_group.borrow_mut();
    mut_new_group.assignments.push(write_en_assn);
    mut_new_group.assignments.push(seek_addr);
    mut_new_group.assignments.push(value_load);
    mut_new_group.assignments.push(done_signal);
    new_group.clone()
}

fn invoke_scan(
    vec_id: &str,
    builder: &mut ir::Builder,
    binding_map: &mut HashMap<String, Rc<RefCell<ir::Cell>>>,
    assignment_map: &mut HashMap<String, Rc<RefCell<ir::Group>>>,
) -> Rc<RefCell<ir::Cell>> {
    let mut scan_ns: frontend::NamespaceDef = frontend::NamespaceDef::construct(&Some(
        "/home/arjun/coursecode/cmsc838l/zinnia/src/calyx-bindings/scan.futil".into(),
    ))
    .unwrap();
    builder.add_component("scan_8", "scan", scan_ns.components[0].signature.clone())
}

fn invoke_filter(
    vec_id: &str,
    builder: &mut ir::Builder,
    binding_map: &mut HashMap<String, Rc<RefCell<ir::Cell>>>,
    assignment_map: &mut HashMap<String, Rc<RefCell<ir::Group>>>,
) -> Rc<RefCell<ir::Cell>> {
    let mut filter_ns: frontend::NamespaceDef = frontend::NamespaceDef::construct(&Some(
        "/home/arjun/coursecode/cmsc838l/zinnia/src/calyx-bindings/filter.futil".into(),
    ))
    .unwrap();
    builder.add_component(
        "filter",
        "filter",
        filter_ns.components[0].signature.clone(),
    )
}

/* Intended for combinational primitives, but maybe can be parameterized to work with anything? */
fn build_binop_assignments(
    builder: &mut ir::Builder,
    binop_prim: Rc<RefCell<ir::Cell>>,
    operand1: &Rc<RefCell<ir::Cell>>,
    operand2: &Rc<RefCell<ir::Cell>>,
    assignment_map: &mut HashMap<String, Rc<RefCell<ir::Group>>>,
    res_size: u64,
) -> Option<Rc<RefCell<ir::Cell>>> {
    let operand1_reg_name: String = operand1.borrow().name().to_string().clone();
    let operand2_reg_name: String = operand2.borrow().name().to_string().clone();
    let operand1_assn: &Rc<RefCell<ir::Group>> = assignment_map.get(&operand1_reg_name).unwrap();
    let operand2_assn: &Rc<RefCell<ir::Group>> = assignment_map.get(&operand2_reg_name).unwrap();
    let binop_res = builder.add_primitive("res_binop", "std_reg", &[res_size]);
    let mut binop_res_assn =
        memory_gen_assignment(builder, None, res_size, &binop_res, Some(&binop_prim));
    build_wire_assignments_comb(
        builder,
        &binop_prim,
        &operand1,
        &operand2,
        &mut binop_res_assn,
    );

    let seq = ir::Control::seq(vec![
        ir::Control::enable(operand1_assn.to_owned()),
        ir::Control::enable(operand2_assn.to_owned()),
        ir::Control::enable(binop_res_assn),
    ]);
    builder.component.control = Rc::new(seq.into());
    Some(binop_res)
}

// note to self: 'sign' bool in NType refers to the int type (either i64 or u64), 'signed' bool on the NumI refers to whether it should be negative
fn memory_gen_value(
    prim_val: Value,
    builder: &mut ir::Builder,
    assignment_map: &mut HashMap<String, Rc<RefCell<ir::Group>>>,
) -> Option<Rc<RefCell<ir::Cell>>> {
    match prim_val {
        Value::Prim(p) => memory_gen_prim(p, builder, assignment_map),
        Value::Vec(lst) => memory_gen_vector(lst.as_ref(), builder),
    }
}

fn memory_gen_vector(
    lst: &[Spanned<Prim>],
    builder: &mut ir::Builder,
) -> Option<Rc<RefCell<ir::Cell>>> {
    let mut data_vals: Vec<i64> = vec![];
    let mut max_width: u8 = 0;
    let mut is_signed_number = false;
    for (exp, _) in lst.iter() {
        match exp {
            Prim::NumI(size_opt, signed_val, unsigned_int_val) => {
                let n_size = size_opt.as_ref().unwrap_or(&NType {
                    sign: false,
                    width: 0,
                });
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
            }
            Prim::NumF(_float_val) => {
                println!("Float element in vector compilation is unimplemented")
            }
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
    let vec_component = builder.add_primitive(
        "mem",
        "comb_mem_d1",
        &[max_width as u64, lst.len() as u64, max_width.ilog2() as u64],
    );
    vec_component
        .borrow_mut()
        .add_attribute(ir::BoolAttr::External, 1);
    return Some(vec_component);
}

// TODO: generate assignments after register allocation
fn memory_gen_prim(
    prim_val: Prim,
    builder: &mut ir::Builder,
    assignment_map: &mut HashMap<String, Rc<RefCell<ir::Group>>>,
) -> Option<Rc<RefCell<ir::Cell>>> {
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
                        let reg_name = new_reg.borrow().name().to_string();
                        let assignment = memory_gen_assignment(
                            builder,
                            Some(int_val),
                            size.width as u64,
                            &new_reg,
                            None,
                        );
                        assignment_map.insert(reg_name, assignment);
                        Some(new_reg)
                    }
                }
                None => {
                    // TODO: Figure out how to handle signed ints since constant! takes u64s
                    if signed {
                        let new_reg = builder.add_primitive("reg", "std_reg", &[8]);
                        Some(new_reg)
                    } else {
                        let new_reg = builder.add_primitive("reg", "std_reg", &[8]);
                        let reg_name = new_reg.borrow().name().to_string();
                        let assignment =
                            memory_gen_assignment(builder, Some(int_val), 8u64, &new_reg, None);
                        assignment_map.insert(reg_name, assignment);
                        Some(new_reg)
                    }
                }
            }
        }
        Prim::Bool(_b_val) => {
            let new_reg = builder.add_primitive("reg", "std_reg", &[1]);
            let reg_name = new_reg.borrow().name().to_string();
            let assignment = memory_gen_assignment(builder, Some(1), 1u64, &new_reg, None);
            assignment_map.insert(reg_name, assignment);
            Some(new_reg)
        }
        Prim::NumF(_f_val) => Some(builder.add_primitive("reg", "std_reg", &[8])), // Warning: floats are not preserved in compilation
    }
}

fn memory_gen_assignment(
    builder: &mut ir::Builder,
    int_val: Option<u64>,
    size: u64,
    new_reg: &Rc<RefCell<ir::Cell>>,
    src_reg_opt: Option<&Rc<RefCell<ir::Cell>>>,
) -> Rc<RefCell<ir::Group>> {
    // Constant signal
    if int_val.is_none() {
        assert!(src_reg_opt.is_some());
    } else {
        assert!(src_reg_opt.is_none());
    }
    let mut assignment_label: String = new_reg.borrow().name().to_string();
    match src_reg_opt {
        Some(src_reg_raw) => {
            structure!(builder;
                let signal_on = constant(1, 1);
            );
            let value_load = src_reg_raw;
            assignment_label.push_str("_reg_assn");
            build_wire_assignments(builder, new_reg, &signal_on, value_load, &assignment_label)
        }
        None => {
            structure!(builder;
                let signal_on = constant(1, 1);
                let value_load = constant(int_val.unwrap(), size);
            );
            assignment_label.push_str("_load");
            build_wire_assignments(builder, new_reg, &signal_on, &value_load, &assignment_label)
        }
    }
}

fn build_wire_assignments(
    builder: &mut ir::Builder,
    new_reg: &Rc<RefCell<ir::Cell>>,
    signal_on: &Rc<RefCell<ir::Cell>>,
    value_load: &Rc<RefCell<ir::Cell>>,
    group_label: &str,
) -> Rc<RefCell<ir::Group>> {
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
        ir::Guard::True,
    );
    let mut mut_new_group = new_group.borrow_mut();
    mut_new_group.assignments.push(write_en_assn);
    mut_new_group.assignments.push(value_load);
    mut_new_group.assignments.push(done_signal);
    new_group.clone()
}

fn build_wire_assignments_comb(
    builder: &mut ir::Builder,
    new_reg: &Rc<RefCell<ir::Cell>>,
    left_value_load: &Rc<RefCell<ir::Cell>>,
    right_value_load: &Rc<RefCell<ir::Cell>>,
    prepend_group: &mut Rc<RefCell<ir::Group>>,
) {
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

    prepend_group
        .borrow_mut()
        .assignments
        .insert(0, left_value_load_assn);
    prepend_group
        .borrow_mut()
        .assignments
        .insert(0, right_value_load_assn);
}
