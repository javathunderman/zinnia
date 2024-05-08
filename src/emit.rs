use calyx_frontend as frontend;
use calyx_ir as ir;
use calyx_ir::structure;
use ir::Assignment;
use serde_json::json;
use std::borrow::BorrowMut;
use std::path::{Path, PathBuf};
use std::{cell::RefCell, collections::HashMap, ops::Deref, rc::Rc};

use crate::types::{SExpr, SExpr_};
use crate::{ast::*, Spanned};

pub fn emit(ast: &SExpr) -> Result<(ir::Context, serde_json::Value), calyx_utils::Error> {
    let cargo = Path::new(env!("CARGO_MANIFEST_DIR"));

    let paths = [
        "./calyx/primitives/memories/comb.futil",
        "./calyx/primitives/core.futil",
        "./src/calyx-bindings/scan.futil",
        "./src/calyx-bindings/list_filter.futil",
    ]
    .iter()
    .map(|p| Path::join(cargo, p))
    .map(|p| Path::canonicalize(p.as_path()))
    .collect::<Result<Vec<PathBuf>, std::io::Error>>()?;

    let mut ws = frontend::Workspace::construct_with_all_deps::<false>(
        paths,
        Path::canonicalize(&Path::join(cargo, "calyx"))
            .expect("Missing calyx! Did you clone the submodule?")
            .as_path(),
    )?;

    let main: frontend::ast::ComponentDef =
        frontend::ast::ComponentDef::new(ir::Id::new("main"), false, None, vec![]);

    ws.components.push(main);

    let mut ctx = ir::from_ast::ast_to_ir(ws)?;
    let main_component = &mut ctx.components[2];

    let mut calyx_builder = ir::Builder::new(main_component, &ctx.lib);
    let mut binding_map: HashMap<String, Rc<RefCell<ir::Cell>>> = HashMap::new();
    let mut assignment_map: HashMap<String, Rc<RefCell<ir::Group>>> = HashMap::new();
    let mut data_json = json!({});
    let res_register = memory_gen(
        &ast,
        &mut calyx_builder,
        &mut binding_map,
        &mut assignment_map,
        &mut data_json,
    )
    .expect("nope");
    let output_width = res_register.borrow().get_parameter("WIDTH").unwrap();
    let mut output_vec = calyx_builder.add_primitive(
        "output_mem",
        "comb_mem_d1",
        &[output_width, 1, output_width],
    );
    RefCell::borrow_mut(output_vec.borrow_mut())
        .add_attribute(ir::Attribute::Bool(ir::BoolAttr::External), 1);
    let output_write_group = build_wire_to_memory(
        &mut calyx_builder,
        &res_register,
        &output_vec,
        output_width,
        0,
        "write_output",
    );
    let mut output_reg_json = json!({
            output_vec.borrow().name().to_string() : {
                "data": [0],
                "format": {
                    "numeric_type": "bitnum",
                    "is_signed": false,
                    "width": output_width
                }
            }
        }
    );
    data_json
        .as_object_mut()
        .unwrap()
        .append(output_reg_json.as_object_mut().unwrap());
    {
        let mut seq = RefCell::borrow_mut(&calyx_builder.component.control);
        match &mut *seq {
            ir::Control::Seq(seq_lst) => {
                seq_lst.stmts.push(ir::Control::enable(output_write_group))
            }
            _ => panic!("no support for non-sequential programs yet (other than parscan)"),
        }
    }
    return Ok((ctx, data_json));
}

fn memory_gen(
    ast: &SExpr,
    builder: &mut ir::Builder,
    binding_map: &mut HashMap<String, Rc<RefCell<ir::Cell>>>,
    assignment_map: &mut HashMap<String, Rc<RefCell<ir::Group>>>,
    data_json: &mut serde_json::Value,
) -> Option<Rc<RefCell<ir::Cell>>> {
    match &ast.expr {
        SExpr_::Value(val) => {
            memory_gen_value(&ast.ty, val.clone(), builder, assignment_map, data_json)
        }
        SExpr_::Id(var_id) => Some(Rc::clone(binding_map.get(var_id).unwrap())),
        SExpr_::Let(binding_lst, rem_expr) => {
            for binding_obj in binding_lst.iter() {
                if !binding_map.contains_key(&binding_obj.id) {
                    /* Compile the terms in the let scope */
                    let result_cell = memory_gen(
                        &binding_obj.expr,
                        builder,
                        binding_map,
                        assignment_map,
                        data_json,
                    );
                    binding_map.insert(binding_obj.id.clone(), result_cell.unwrap());
                } else {
                    panic!("ID in let binding has been reused");
                }
            }
            /* Compile terms outside of the let scope */
            memory_gen(
                rem_expr.as_ref(),
                builder,
                binding_map,
                assignment_map,
                data_json,
            )
        }
        SExpr_::Binary(rem_expr_1, operator, rem_expr_2) => Some(
            memory_gen_binop(
                rem_expr_1.as_ref(),
                binding_map,
                rem_expr_2.as_ref(),
                &operator,
                builder,
                assignment_map,
                data_json,
            )
            .2,
        ),
        SExpr_::Call(func_expr, func_args) => match &func_expr.expr {
            SExpr_::Id(func_name) => match func_name.as_str() {
                "filter" => {
                    if let Some(SExpr_::Id(vec_id)) = &func_args.get(0).map(|x| &x.expr) {
                        Some(invoke_filter(&vec_id, builder, binding_map, assignment_map))
                    } else {
                        panic!("Not a vector variable, try using a let binding")
                    }
                }
                "scan" => {
                    if let Some(SExpr_::Id(vec_id)) = &func_args.get(0).map(|x| &x.expr) {
                        Some(invoke_scan(&vec_id, builder, binding_map, assignment_map))
                    } else {
                        panic!("Not a vector variable, try using a let binding")
                    }
                }
                _ => panic!("Unknown function name in call"),
            },
            _ => todo!("Indirect function references not yet implemented!"),
        },
        SExpr_::If(condition, true_body, false_body) => match &condition.expr {
            SExpr_::Binary(rem_expr1, op, rem_expr2) => {
                match op {
                    BinaryOp::Add => panic!("Invalid binop in if statement"),
                    BinaryOp::Sub => panic!("Invalid binop in if statement"),
                    BinaryOp::Mul => panic!("Invalid binop in if statement"),
                    BinaryOp::Div => panic!("Invalid binop in if statement"),
                    _ => dbg!("OK binop in if statement (can be made into comb group)"),
                };
                let condition_expr_res = memory_gen_binop(
                    &rem_expr1,
                    binding_map,
                    &rem_expr2,
                    &op,
                    builder,
                    assignment_map,
                    data_json,
                );
                let mut control_group = condition_expr_res.1.unwrap();
                let right_assn = RefCell::borrow_mut(control_group.borrow_mut())
                    .assignments
                    .get(0)
                    .unwrap()
                    .to_owned();
                let left_assn = RefCell::borrow_mut(control_group.borrow_mut())
                    .assignments
                    .get(1)
                    .unwrap()
                    .to_owned();
                RefCell::borrow_mut(control_group.borrow_mut())
                    .assignments
                    .remove(0);
                RefCell::borrow_mut(control_group.borrow_mut())
                    .assignments
                    .remove(0);
                let true_cell =
                    memory_gen(&true_body, builder, binding_map, assignment_map, data_json)
                        .unwrap();
                let false_cell =
                    memory_gen(&false_body, builder, binding_map, assignment_map, data_json)
                        .unwrap();

                let cond_group = builder.add_comb_group("if_expr_c");
                cond_group
                    .deref()
                    .borrow_mut()
                    .assignments
                    .insert(0, left_assn);
                cond_group
                    .deref()
                    .borrow_mut()
                    .assignments
                    .insert(0, right_assn);

                let true_group = assignment_map.get(&true_cell.borrow().name().to_string());
                let false_group = assignment_map.get(&false_cell.borrow().name().to_string());
                let true_seq = Box::new(ir::Control::enable(true_group.unwrap().to_owned()));
                let false_seq = Box::new(ir::Control::enable(false_group.unwrap().to_owned()));
                let seq = ir::Control::if_(
                    condition_expr_res.0.unwrap().borrow().get("out"),
                    Some(cond_group),
                    true_seq,
                    false_seq,
                );
                builder.component.control = Rc::new(seq.into());
                None
            }
            _ => panic!("If statement did not have binop that is a comparator"),
        },
        _ => None,
    }
}

fn memory_gen_binop(
    rem_expr_1: &SExpr,
    binding_map: &mut HashMap<String, Rc<RefCell<ir::Cell>>>,
    rem_expr_2: &SExpr,
    operator: &BinaryOp,
    builder: &mut ir::Builder,
    assignment_map: &mut HashMap<String, Rc<RefCell<ir::Group>>>,
    data_json: &mut serde_json::Value,
) -> (
    Option<Rc<RefCell<ir::Cell>>>,
    Option<Rc<RefCell<ir::Group>>>,
    Rc<RefCell<ir::Cell>>,
) {
    let operand1 : &Rc<RefCell<ir::Cell>> = match &rem_expr_1.expr {
        SExpr_::Id(var_name) => binding_map.get(var_name.as_str()).unwrap(),
        _ => panic!("Cannot compile inlined expressions right now, use a let binding and create the var first")
    };
    let operand2: &Rc<RefCell<ir::Cell>> = match &rem_expr_2.expr {
        SExpr_::Id(var_name) => binding_map.get(var_name.as_str()).unwrap(),
        _ => {
            dbg!(&rem_expr_2);
            panic!("Cannot compile inlined expressions right now, use a let binding and create the var first")
        }
    };

    let size = operand1.borrow().get_parameter("WIDTH").unwrap();
    // assuming the typechecker will catch non-matching bit widths
    let mut res_size = size;
    let mut drive_go = false;
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
        BinaryOp::Mul => {
            drive_go = true;
            builder.add_primitive("mult", "std_fp_mult_pipe", &[size, (size / 2), (size / 2)])
        }
        BinaryOp::Div => {
            drive_go = true;
            builder.add_primitive("div", "std_fp_div_pipe", &[size, (size / 2), (size / 2)])
        }
        _ => panic!("Unimplemented binop"),
    };
    let control_group = build_binop_assignments(
        builder,
        &binop_prim,
        operand1,
        operand2,
        assignment_map,
        res_size,
        drive_go,
    );
    memory_gen(&rem_expr_2, builder, binding_map, assignment_map, data_json);
    (Some(binop_prim), control_group.0, control_group.1)
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
        let read_from_addr = constant(vec_index, bit_width);
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

    new_group
        .deref()
        .borrow_mut()
        .assignments
        .push(write_en_assn);
    new_group.deref().borrow_mut().assignments.push(seek_addr);
    new_group.deref().borrow_mut().assignments.push(value_load);
    new_group.deref().borrow_mut().assignments.push(done_signal);
    new_group.clone()
}

fn build_wire_to_memory(
    builder: &mut ir::Builder,
    src_reg: &Rc<RefCell<ir::Cell>>,
    vec_dest: &Rc<RefCell<ir::Cell>>,
    bit_width: u64,
    vec_index: u64,
    group_label: &str,
) -> Rc<RefCell<ir::Group>> {
    let new_group = builder.add_group(group_label);
    structure!(builder;
        let signal_on = constant(1, 1);
        let write_to_addr = constant(vec_index, bit_width);
    );
    let write_en_assn = builder.build_assignment(
        vec_dest.borrow().get("write_en"),
        signal_on.borrow().get("out"),
        ir::Guard::True,
    );
    let seek_addr = builder.build_assignment(
        vec_dest.borrow().get("addr0"),
        write_to_addr.borrow().get("out"),
        ir::Guard::True,
    );
    let value_load = builder.build_assignment(
        vec_dest.borrow().get("write_data"),
        src_reg.borrow().get("out"),
        ir::Guard::True,
    );
    let done_signal = builder.build_assignment(
        new_group.borrow().get("done"),
        vec_dest.borrow().get("done"),
        ir::Guard::True,
    );

    new_group
        .deref()
        .borrow_mut()
        .assignments
        .push(write_en_assn);
    new_group.deref().borrow_mut().assignments.push(seek_addr);
    new_group.deref().borrow_mut().assignments.push(value_load);
    new_group.deref().borrow_mut().assignments.push(done_signal);
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
    binop_prim: &Rc<RefCell<ir::Cell>>,
    operand1: &Rc<RefCell<ir::Cell>>,
    operand2: &Rc<RefCell<ir::Cell>>,
    assignment_map: &mut HashMap<String, Rc<RefCell<ir::Group>>>,
    res_size: u64,
    drive_go: bool,
) -> (Option<Rc<RefCell<ir::Group>>>, Rc<RefCell<ir::Cell>>) {
    let operand1_reg_name: String = operand1.borrow().name().to_string().clone();
    let operand2_reg_name: String = operand2.borrow().name().to_string().clone();
    let local_assn_map = assignment_map.to_owned();
    let operand1_assn: &Rc<RefCell<ir::Group>> = local_assn_map.get(&operand1_reg_name).unwrap();
    let operand2_assn: &Rc<RefCell<ir::Group>> = local_assn_map.get(&operand2_reg_name).unwrap();
    let binop_res = builder.add_primitive("res_binop", "std_reg", &[res_size]);
    let mut binop_res_assn = memory_gen_assignment(
        builder,
        None,
        res_size,
        &binop_res,
        Some(binop_prim),
        assignment_map,
    );
    build_wire_assignments_comb(
        builder,
        binop_prim,
        &operand1,
        &operand2,
        &mut binop_res_assn,
    );
    if drive_go {
        structure!(builder;
            let signal_on = constant(1, 1);
        );
        let go_signaling = builder.build_assignment(
            binop_prim.borrow().get("go"),
            signal_on.borrow().get("out"),
            ir::Guard::True,
        );
        binop_res_assn
            .deref()
            .borrow_mut()
            .assignments
            .insert(0, go_signaling);
    }
    let seq = ir::Control::seq(vec![
        ir::Control::enable(operand1_assn.to_owned()),
        ir::Control::enable(operand2_assn.to_owned()),
        ir::Control::enable(binop_res_assn.to_owned()),
    ]);
    builder.component.control = Rc::new(seq.into());
    (Some(binop_res_assn), binop_res)
}

// note to self: 'sign' bool in NType refers to the int type (either i64 or u64), 'signed' bool on the NumI refers to whether it should be negative
fn memory_gen_value(
    ty: &Type,
    prim_val: Value,
    builder: &mut ir::Builder,
    assignment_map: &mut HashMap<String, Rc<RefCell<ir::Group>>>,
    data_json: &mut serde_json::Value,
) -> Option<Rc<RefCell<ir::Cell>>> {
    match prim_val {
        Value::Prim(p) => memory_gen_prim(ty, p, builder, assignment_map),
        Value::Vec(lst) => memory_gen_vector(ty, lst.as_ref(), builder, data_json),
    }
}

fn memory_gen_vector(
    ty: &Type,
    lst: &[Spanned<Prim>],
    builder: &mut ir::Builder,
    data_json: &mut serde_json::Value,
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

    let vec_component = builder.add_primitive(
        "mem",
        "comb_mem_d1",
        &[max_width as u64, lst.len() as u64, max_width as u64],
    );
    vec_component
        .deref()
        .borrow_mut()
        .add_attribute(ir::BoolAttr::External, 1);

    let mut data_file = json!({
        vec_component.deref().borrow().name().to_string():
        {
            "data": data_vals,
            "format" : {
                "numeric_type": "bitnum",
                "is_signed" : is_signed_number,
                "width": max_width
            }
        }
    });
    data_json
        .as_object_mut()
        .unwrap()
        .append(data_file.as_object_mut().unwrap());
    dbg!(data_json.to_string());
    return Some(vec_component);
}

// TODO: generate assignments after register allocation
fn memory_gen_prim(
    ty: &Type,
    prim_val: Prim,
    builder: &mut ir::Builder,
    assignment_map: &mut HashMap<String, Rc<RefCell<ir::Group>>>,
) -> Option<Rc<RefCell<ir::Cell>>> {
    let Type::Num(NType { sign, width }) = ty else {
        return None;
    };

    match prim_val {
        Prim::NumI(size_opt, signed, int_val) => {
            // TODO: Figure out how to handle signed ints since constant! takes u64s
            if signed {
                let new_reg = builder.add_primitive("reg", "std_reg", &[*width as u64]);
                Some(new_reg)
            } else {
                let new_reg = builder.add_primitive("reg", "std_reg", &[*width as u64]);
                let reg_name = new_reg.borrow().name().to_string();
                let assignment = memory_gen_assignment(
                    builder,
                    Some(int_val),
                    *width as u64,
                    &new_reg,
                    None,
                    assignment_map,
                );
                assignment_map.insert(reg_name, assignment);
                Some(new_reg)
            }
        }
        Prim::Bool(_b_val) => {
            let new_reg = builder.add_primitive("reg", "std_reg", &[1]);
            let reg_name = new_reg.borrow().name().to_string();
            let assignment =
                memory_gen_assignment(builder, Some(1), 1u64, &new_reg, None, assignment_map);
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
    assignment_map: &mut HashMap<String, Rc<RefCell<ir::Group>>>,
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
            build_wire_assignments(
                builder,
                new_reg,
                &signal_on,
                value_load,
                assignment_map,
                &assignment_label,
            )
        }
        None => {
            structure!(builder;
                let signal_on = constant(1, 1);
                let value_load = constant(int_val.unwrap(), size);
            );
            assignment_label.push_str("_load");
            build_wire_assignments(
                builder,
                new_reg,
                &signal_on,
                &value_load,
                assignment_map,
                &assignment_label,
            )
        }
    }
}

fn build_wire_assignments(
    builder: &mut ir::Builder,
    new_reg: &Rc<RefCell<ir::Cell>>,
    signal_on: &Rc<RefCell<ir::Cell>>,
    value_load: &Rc<RefCell<ir::Cell>>,
    assignment_map: &mut HashMap<String, Rc<RefCell<ir::Group>>>,
    group_label: &str,
) -> Rc<RefCell<ir::Group>> {
    let mut new_group = builder.add_group(group_label);
    let write_en_assn = builder.build_assignment(
        new_reg.borrow().get("write_en"),
        signal_on.borrow().get("out"),
        ir::Guard::True,
    );
    let value_load_res = builder.build_assignment(
        new_reg.borrow().get("in"),
        value_load.borrow().get("out"),
        ir::Guard::True,
    );
    let done_signal = builder.build_assignment(
        new_group.borrow().get("done"),
        new_reg.borrow().get("done"),
        ir::Guard::True,
    );

    new_group
        .deref()
        .borrow_mut()
        .assignments
        .push(write_en_assn);
    new_group
        .deref()
        .borrow_mut()
        .assignments
        .push(value_load_res);
    new_group.deref().borrow_mut().assignments.push(done_signal);
    let returnable = new_group.clone();
    assignment_map.insert(new_reg.borrow().name().to_string(), new_group);
    returnable
}

fn build_wire_assignments_comb(
    builder: &mut ir::Builder,
    new_reg: &Rc<RefCell<ir::Cell>>,
    left_value_load: &Rc<RefCell<ir::Cell>>,
    right_value_load: &Rc<RefCell<ir::Cell>>,
    prepend_group: &mut Rc<RefCell<ir::Group>>,
) {
    let left_value_load_assn: Assignment<ir::Nothing> = builder.build_assignment(
        new_reg.borrow().get("left"),
        left_value_load.borrow().get("out"),
        ir::Guard::True,
    );
    let right_value_load_assn: Assignment<ir::Nothing> = builder.build_assignment(
        new_reg.borrow().get("right"),
        right_value_load.borrow().get("out"),
        ir::Guard::True,
    );

    RefCell::borrow_mut(prepend_group)
        .assignments
        .insert(0, left_value_load_assn);
    RefCell::borrow_mut(prepend_group)
        .assignments
        .insert(1, right_value_load_assn);
}
