use calyx_frontend as frontend;
use calyx_ir as ir;
use calyx_ir::structure;
use ir::{Assignment, Component, LibrarySignatures};
use serde_json::json;
use std::borrow::BorrowMut;
use std::path::{Path, PathBuf};
use std::{cell::RefCell, collections::HashMap, ops::Deref, rc::Rc};

use crate::types::{SExpr, SExpr_};
use crate::{ast::*, Spanned};

struct EmitContext<'a> {
    builder: ir::Builder<'a>,
    binding_map: HashMap<String, Rc<RefCell<ir::Cell>>>,
    assignment_map: HashMap<String, Rc<RefCell<ir::Group>>>,
    data_json: serde_json::Value,
}

impl<'a> EmitContext<'a> {
    pub fn new(lib: &'a LibrarySignatures, main: &'a mut Component) -> Self {
        EmitContext {
            builder: ir::Builder::new(main, lib),
            binding_map: HashMap::new(),
            assignment_map: HashMap::new(),
            data_json: json!({}),
        }
    }

    fn memory_gen(&mut self, ast: &SExpr) -> Option<Rc<RefCell<ir::Cell>>> {
        match &ast.expr {
            SExpr_::Value(val) => self.memory_gen_value(&ast.ty, val.clone()),
            SExpr_::Id(var_id) => Some(Rc::clone(self.binding_map.get(var_id).unwrap())),
            SExpr_::Let(binding_lst, rem_expr) => {
                for binding_obj in binding_lst.iter() {
                    if !self.binding_map.contains_key(&binding_obj.id) {
                        /* Compile the terms in the let scope */
                        let result_cell = self.memory_gen(&binding_obj.expr);
                        self.binding_map
                            .insert(binding_obj.id.clone(), result_cell.unwrap());
                    } else {
                        panic!("ID in let binding has been reused");
                    }
                }
                /* Compile terms outside of the let scope */
                self.memory_gen(rem_expr.as_ref())
            }
            SExpr_::Binary(rem_expr_1, operator, rem_expr_2) => Some(
                self.memory_gen_binop(rem_expr_1.as_ref(), rem_expr_2.as_ref(), &operator)
                    .2,
            ),
            SExpr_::Call(func_expr, func_args) => match &func_expr.expr {
                SExpr_::Id(func_name) => match func_name.as_str() {
                    "filter" => {
                        if let Some(SExpr_::Id(vec_id)) = &func_args.get(0).map(|x| &x.expr) {
                            Some(self.invoke_filter(&vec_id))
                        } else {
                            panic!("Not a vector variable, try using a let binding")
                        }
                    }
                    "scan" => {
                        if let Some(SExpr_::Id(vec_id)) = &func_args.get(0).map(|x| &x.expr) {
                            Some(self.invoke_scan(&vec_id))
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

                    let condition_expr_res = self.memory_gen_binop(&rem_expr1, &rem_expr2, &op);

                    let (left_assn, right_assn) = {
                        let mut rc = condition_expr_res.1.unwrap();
                        let mut control_group = RefCell::borrow_mut(rc.borrow_mut());

                        (
                            control_group.assignments.remove(1),
                            control_group.assignments.remove(0),
                        )
                    };

                    let true_cell = self.memory_gen(&true_body).unwrap();
                    let false_cell = self.memory_gen(&false_body).unwrap();

                    let cond_group = self.builder.add_comb_group("if_expr_c");
                    cond_group
                        .deref()
                        .borrow_mut()
                        .assignments
                        .extend([left_assn, right_assn]);

                    let true_group = self
                        .assignment_map
                        .get(&true_cell.borrow().name().to_string());

                    let false_group = self
                        .assignment_map
                        .get(&false_cell.borrow().name().to_string());

                    let true_seq = Box::new(ir::Control::enable(true_group.unwrap().to_owned()));
                    let false_seq = Box::new(ir::Control::enable(false_group.unwrap().to_owned()));

                    let seq = ir::Control::if_(
                        condition_expr_res.0.unwrap().borrow().get("out"),
                        Some(cond_group),
                        true_seq,
                        false_seq,
                    );

                    self.builder.component.control = Rc::new(seq.into());

                    None
                }
                _ => panic!("If statement did not have binop that is a comparator"),
            },
        }
    }

    fn memory_gen_binop(
        &mut self,
        rem_expr_1: &SExpr,
        rem_expr_2: &SExpr,
        operator: &BinaryOp,
    ) -> (
        Option<Rc<RefCell<ir::Cell>>>,
        Option<Rc<RefCell<ir::Group>>>,
        Rc<RefCell<ir::Cell>>,
    ) {
        let operand1 = match &rem_expr_1.expr {
            SExpr_::Id(var_name) => self.binding_map.get(var_name.as_str()).unwrap().clone(),
            _ => panic!("Cannot compile inlined expressions right now, use a let binding and create the var first")
        };
        let operand2 = match &rem_expr_2.expr {
            SExpr_::Id(var_name) => self.binding_map.get(var_name.as_str()).unwrap().clone(),
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
            BinaryOp::Add => self.builder.add_primitive("adder", "std_add", &[size]),
            BinaryOp::Sub => self.builder.add_primitive("subtract", "std_sub", &[size]),
            BinaryOp::Eq => {
                res_size = 1;
                self.builder.add_primitive("equality", "std_eq", &[size])
            }
            BinaryOp::Leq => {
                res_size = 1;
                self.builder.add_primitive("leq", "std_le", &[size])
            }
            BinaryOp::Geq => {
                res_size = 1;
                self.builder.add_primitive("geq", "std_ge", &[size])
            }
            BinaryOp::Lt => {
                res_size = 1;
                self.builder.add_primitive("lt", "std_lt", &[size])
            }
            BinaryOp::Gt => {
                res_size = 1;
                self.builder.add_primitive("gt", "std_gt", &[size])
            }
            BinaryOp::NotEq => {
                res_size = 1;
                self.builder.add_primitive("neq", "std_neq", &[size])
            }
            BinaryOp::Mul => {
                drive_go = true;
                self.builder.add_primitive(
                    "mult",
                    "std_fp_mult_pipe",
                    &[size, (size / 2), (size / 2)],
                )
            }
            BinaryOp::Div => {
                drive_go = true;
                self.builder.add_primitive(
                    "div",
                    "std_fp_div_pipe",
                    &[size, (size / 2), (size / 2)],
                )
            }
        };

        let control_group =
            self.build_binop_assignments(&binop_prim, &operand1, &operand2, res_size, drive_go);
        self.memory_gen(&rem_expr_2);

        (Some(binop_prim), control_group.0, control_group.1)
    }

    fn build_wire_from_memory(
        &mut self,
        new_reg: &Rc<RefCell<ir::Cell>>,
        vec_origin: &Rc<RefCell<ir::Cell>>,
        bit_width: u64,
        vec_index: u64,
        group_label: &str,
    ) -> Rc<RefCell<ir::Group>> {
        let new_group = self.builder.add_group(group_label);
        structure!(self.builder;
            let signal_on = constant(1, 1);
            let read_from_addr = constant(vec_index, bit_width);
        );
        let write_en_assn = self.builder.build_assignment(
            new_reg.borrow().get("write_en"),
            signal_on.borrow().get("out"),
            ir::Guard::True,
        );
        let seek_addr = self.builder.build_assignment(
            vec_origin.borrow().get("addr0"),
            read_from_addr.borrow().get("out"),
            ir::Guard::True,
        );
        let value_load = self.builder.build_assignment(
            new_reg.borrow().get("in"),
            vec_origin.borrow().get("read_data"),
            ir::Guard::True,
        );
        let done_signal = self.builder.build_assignment(
            new_group.borrow().get("done"),
            new_reg.borrow().get("done"),
            ir::Guard::True,
        );

        new_group.deref().borrow_mut().assignments.extend([
            write_en_assn,
            seek_addr,
            value_load,
            done_signal,
        ]);

        new_group.clone()
    }

    fn build_wire_to_memory(
        &mut self,
        src_reg: &Rc<RefCell<ir::Cell>>,
        vec_dest: &Rc<RefCell<ir::Cell>>,
        bit_width: u64,
        vec_index: u64,
        group_label: &str,
    ) -> Rc<RefCell<ir::Group>> {
        let new_group = self.builder.add_group(group_label);
        structure!(self.builder;
            let signal_on = constant(1, 1);
            let write_to_addr = constant(vec_index, bit_width);
        );
        let write_en_assn = self.builder.build_assignment(
            vec_dest.borrow().get("write_en"),
            signal_on.borrow().get("out"),
            ir::Guard::True,
        );
        let seek_addr = self.builder.build_assignment(
            vec_dest.borrow().get("addr0"),
            write_to_addr.borrow().get("out"),
            ir::Guard::True,
        );
        let value_load = self.builder.build_assignment(
            vec_dest.borrow().get("write_data"),
            src_reg.borrow().get("out"),
            ir::Guard::True,
        );
        let done_signal = self.builder.build_assignment(
            new_group.borrow().get("done"),
            vec_dest.borrow().get("done"),
            ir::Guard::True,
        );

        new_group.deref().borrow_mut().assignments.extend([
            write_en_assn,
            seek_addr,
            value_load,
            done_signal,
        ]);

        new_group.clone()
    }

    fn invoke_scan(&mut self, vec_id: &str) -> Rc<RefCell<ir::Cell>> {
        let mut scan_ns: frontend::NamespaceDef = frontend::NamespaceDef::construct(&Some(
            "/home/arjun/coursecode/cmsc838l/zinnia/src/calyx-bindings/scan.futil".into(),
        ))
        .unwrap();
        self.builder
            .add_component("scan_8", "scan", scan_ns.components[0].signature.clone())
    }

    fn invoke_filter(&mut self, vec_id: &str) -> Rc<RefCell<ir::Cell>> {
        let mut filter_ns: frontend::NamespaceDef = frontend::NamespaceDef::construct(&Some(
            "/home/arjun/coursecode/cmsc838l/zinnia/src/calyx-bindings/filter.futil".into(),
        ))
        .unwrap();
        self.builder.add_component(
            "filter",
            "filter",
            filter_ns.components[0].signature.clone(),
        )
    }

    /* Intended for combinational primitives, but maybe can be parameterized to work with anything? */
    fn build_binop_assignments(
        &mut self,
        binop_prim: &Rc<RefCell<ir::Cell>>,
        operand1: &Rc<RefCell<ir::Cell>>,
        operand2: &Rc<RefCell<ir::Cell>>,
        res_size: u64,
        drive_go: bool,
    ) -> (Option<Rc<RefCell<ir::Group>>>, Rc<RefCell<ir::Cell>>) {
        let operand1_reg_name = operand1.borrow().name().to_string().clone();
        let operand2_reg_name = operand2.borrow().name().to_string().clone();
        let operand1_assn = self.assignment_map.get(&operand1_reg_name).unwrap().clone();
        let operand2_assn = self.assignment_map.get(&operand2_reg_name).unwrap().clone();
        let binop_res = self
            .builder
            .add_primitive("res_binop", "std_reg", &[res_size]);
        let mut binop_res_assn =
            self.memory_gen_assignment(None, res_size, &binop_res, Some(binop_prim));
        self.build_wire_assignments_comb(binop_prim, &operand1, &operand2, &mut binop_res_assn);
        if drive_go {
            structure!(self.builder;
                let signal_on = constant(1, 1);
            );
            let go_signaling = self.builder.build_assignment(
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
        self.builder.component.control = Rc::new(seq.into());
        (Some(binop_res_assn), binop_res)
    }

    // note to self: 'sign' bool in NType refers to the int type (either i64 or u64), 'signed' bool on the NumI refers to whether it should be negative
    fn memory_gen_value(&mut self, ty: &Type, prim_val: Value) -> Option<Rc<RefCell<ir::Cell>>> {
        match prim_val {
            Value::Prim(p) => self.memory_gen_prim(ty, p),
            Value::Vec(lst) => self.memory_gen_vector(ty, lst.as_ref()),
        }
    }

    fn memory_gen_vector(
        &mut self,
        ty: &Type,
        lst: &[Spanned<Prim>],
    ) -> Option<Rc<RefCell<ir::Cell>>> {
        let Type::VecT(VecT { elem_t, count }) = ty else {
            panic!("Vector not vector typed!")
        };

        let Type::Num(NType { sign, width }) = elem_t.as_ref() else {
            panic!("Vector contains non-numeric elements!");
        };

        let mut data_vals: Vec<i64> = vec![];
        let mut max_width: u8 = 0;
        let mut is_signed_number = false;
        for (exp, _) in lst.iter() {
            match exp {
                Prim::NumI(_, signed_val, unsigned_int_val) => {
                    is_signed_number |= *sign;

                    max_width = max_width.max(*width);

                    let int_val = *unsigned_int_val as i64;

                    if *signed_val {
                        data_vals.push(-int_val);
                    } else {
                        data_vals.push(int_val);
                    }
                }
                Prim::Bool(b_val) => {
                    max_width = 1;
                    data_vals.push(*b_val as i64);
                }
                Prim::NumF(_float_val) => {
                    println!("Float element in vector compilation is unimplemented")
                }
            }
        }

        if max_width == 0 {
            panic!("Max width of 0! Is the vector empty?");
        }

        let vec_component = self.builder.add_primitive(
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

        self.data_json
            .as_object_mut()
            .unwrap()
            .append(data_file.as_object_mut().unwrap());

        dbg!(self.data_json.to_string());

        return Some(vec_component);
    }

    fn add_reg(&mut self, val: u64, width: u64) -> Rc<RefCell<ir::Cell>> {
        let new_reg = self
            .builder
            .add_primitive("reg", "std_reg", &[width as u64]);

        let assignment = self.memory_gen_assignment(Some(val), width, &new_reg, None);
        self.assignment_map
            .insert(new_reg.borrow().name().to_string(), assignment);

        new_reg
    }

    // TODO: generate assignments after register allocation
    fn memory_gen_prim(&mut self, ty: &Type, prim_val: Prim) -> Option<Rc<RefCell<ir::Cell>>> {
        let Type::Num(NType { sign, width }) = ty else {
            return None;
        };

        match prim_val {
            Prim::NumI(_, signed, int_val) => {
                // TODO: Figure out how to handle signed ints since constant! takes u64s
                if signed {
                    let new_reg = self
                        .builder
                        .add_primitive("reg", "std_reg", &[*width as u64]);

                    Some(new_reg)
                } else {
                    Some(self.add_reg(int_val, *width as u64))
                }
            }

            Prim::Bool(_b_val) => Some(self.add_reg(1, 1)),

            // Warning: floats are not preserved in compilation
            Prim::NumF(_f_val) => Some(self.builder.add_primitive("reg", "std_reg", &[8])),
        }
    }

    fn memory_gen_assignment(
        &mut self,
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

        let signal_on = self.builder.add_constant(1, 1);

        let value_load = match src_reg_opt {
            Some(src_reg_raw) => {
                let value_load = src_reg_raw;
                assignment_label.push_str("_reg_assn");

                value_load.clone()
            }
            None => {
                structure!((self.builder);
                    let value_load = constant(int_val.unwrap(),size);
                );
                assignment_label.push_str("_load");

                value_load
            }
        };

        self.build_wire_assignments(new_reg, &signal_on, &value_load, &assignment_label)
    }

    fn build_wire_assignments(
        &mut self,
        new_reg: &Rc<RefCell<ir::Cell>>,
        signal_on: &Rc<RefCell<ir::Cell>>,
        value_load: &Rc<RefCell<ir::Cell>>,
        group_label: &str,
    ) -> Rc<RefCell<ir::Group>> {
        let new_group = self.builder.add_group(group_label);

        let write_en_assn = self.builder.build_assignment(
            new_reg.borrow().get("write_en"),
            signal_on.borrow().get("out"),
            ir::Guard::True,
        );
        let value_load_res = self.builder.build_assignment(
            new_reg.borrow().get("in"),
            value_load.borrow().get("out"),
            ir::Guard::True,
        );
        let done_signal = self.builder.build_assignment(
            new_group.borrow().get("done"),
            new_reg.borrow().get("done"),
            ir::Guard::True,
        );

        new_group.deref().borrow_mut().assignments.extend([
            write_en_assn,
            value_load_res,
            done_signal,
        ]);

        let returnable = new_group.clone();

        self.assignment_map
            .insert(new_reg.borrow().name().to_string(), new_group);

        returnable
    }

    fn build_wire_assignments_comb(
        &self,
        new_reg: &Rc<RefCell<ir::Cell>>,
        left_value_load: &Rc<RefCell<ir::Cell>>,
        right_value_load: &Rc<RefCell<ir::Cell>>,
        prepend_group: &mut Rc<RefCell<ir::Group>>,
    ) {
        let left_value_load_assn: Assignment<ir::Nothing> = self.builder.build_assignment(
            new_reg.borrow().get("left"),
            left_value_load.borrow().get("out"),
            ir::Guard::True,
        );

        let right_value_load_assn: Assignment<ir::Nothing> = self.builder.build_assignment(
            new_reg.borrow().get("right"),
            right_value_load.borrow().get("out"),
            ir::Guard::True,
        );

        let mut prepend_group = RefCell::borrow_mut(prepend_group);

        prepend_group.assignments.insert(0, left_value_load_assn);
        prepend_group.assignments.insert(1, right_value_load_assn);
    }
}

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

    let mut emit = EmitContext::new(&ctx.lib, main_component);

    let res_register = emit.memory_gen(&ast).expect("nope");
    let output_width = res_register.borrow().get_parameter("WIDTH").unwrap();
    let mut output_vec = emit.builder.add_primitive(
        "output_mem",
        "comb_mem_d1",
        &[output_width, 1, output_width],
    );
    RefCell::borrow_mut(output_vec.borrow_mut())
        .add_attribute(ir::Attribute::Bool(ir::BoolAttr::External), 1);
    let output_write_group =
        emit.build_wire_to_memory(&res_register, &output_vec, output_width, 0, "write_output");
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
    emit.data_json
        .as_object_mut()
        .unwrap()
        .append(output_reg_json.as_object_mut().unwrap());
    {
        let mut seq = RefCell::borrow_mut(&emit.builder.component.control);
        match &mut *seq {
            ir::Control::Seq(seq_lst) => {
                seq_lst.stmts.push(ir::Control::enable(output_write_group))
            }
            _ => panic!("no support for non-sequential programs yet (other than parscan)"),
        }
    }

    let json = emit.data_json;

    Ok((ctx, json))
}
