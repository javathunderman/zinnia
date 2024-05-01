mod ast;
mod parse;

use ariadne::{sources, Color, Label, Report, ReportKind};
use chumsky::prelude::*;
use calyx_ir as ir;
use calyx_frontend as frontend;
use std::collections::HashSet;
use std::io::Write;
use serde_json::json;

use ast::*;
use parse::*;

pub type Span = SimpleSpan<usize>;

pub type Spanned<T> = (T, Span);

pub type ParserInput<'tokens, 'src> = chumsky::input::SpannedInput<Token<'src>, Span, &'tokens [(Token<'src>, Span)]>;

fn main() -> Result<(), std::io::Error> {
    let mut args = std::env::args();

    if args.len() < 2 {
        println!("Usage: [cargo run] FILE");
        return Ok(());
    }

    let filename = args.nth(1).unwrap();

    let src = std::fs::read_to_string(filename.clone()).expect("Unable to read source!");

    let (tokens, errs) = lexer().parse(src.as_str()).into_output_errors();

    let parse_errs = if let Some(tokens) = &tokens {
        let (ast, parse_errs) = expr_parser()
            .map_with(|ast, e| (ast, e.span()))
            .parse(tokens.as_slice().spanned((src.len()..src.len()).into()))
            .into_output_errors();
        dbg!(&ast);
        let ctx = match emit(ast) {
            Ok(v) => v,
            Err(e) => {
                let (file, start, end) = {
                    let (file, start, end) = e.location();

                    (file.to_owned(), start, end)

                };

                Report::build(ReportKind::Error, file.clone(), start)
                    .with_message(e.message())
                    .with_label(
                        Label::new((file.clone(), start..end))
                            .with_message(e.message())
                            .with_color(Color::Red)
                    )
                    .finish()
                    .print(sources([(file.clone(), &src)]))
                    .expect("stdout io err");

                return Err(std::io::Error::new(std::io::ErrorKind::Other, "oh no!"));
            }
        };

        let out = &mut std::io::stdout();

        for comp in &ctx.components {
            ir::Printer::write_component(comp, out).expect("stdout io err");
            writeln!(out);
        }

        parse_errs
    } else {
        Vec::new()
    };

    errs.into_iter()
        .map(|e| e.map_token(|c| c.to_string()))
        .chain(
            parse_errs
                .into_iter()
                .map(|e| e.map_token(|tok| tok.to_string())),
        )
        .for_each(|e| {
            Report::build(ReportKind::Error, filename.clone(), e.span().start)
                .with_message(e.to_string())
                .with_label(
                    Label::new((filename.clone(), e.span().into_range()))
                        .with_message(e.reason().to_string())
                        .with_color(Color::Red),
                )
                .with_labels(e.contexts().map(|(label, span)| {
                    Label::new((filename.clone(), span.into_range()))
                        .with_message(format!("while parsing this {}", label))
                        .with_color(Color::Yellow)
                }))
                .finish()
                .print(sources([(filename.clone(), src.clone())]))
                .unwrap()
        });

    Ok(())
}

fn emit(ast: Option<((Expr<'_>, SimpleSpan), SimpleSpan)>) -> Result<ir::Context, calyx_utils::Error> {
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

// note to self: 'sign' bool in NSize refers to the int type (either i64 or u64), 'signed' bool on the NumI refers to whether it should be negative
fn memory_gen_value(prim_val: Value, builder: &mut ir::Builder) {
    match prim_val {
        Value::Prim(p) => memory_gen_prim(p, builder),
        Value::Vec(lst) => memory_gen_vector(lst, builder),
        _ => println!("Unimplemented")
    }
}

fn memory_gen_vector(lst: Box<[Value]>, builder: &mut ir::Builder) {
    let mut data_vals: Vec<i64> = vec![];
    let mut max_width: i8 = 0;
    let mut is_signed_number = false;
    for val in lst.iter() {
        match val {
            Value::Prim(exp) => {
                match exp {
                    Prim::NumI(size_opt, signed_val, unsigned_int_val) => {
                            let n_size = size_opt.as_ref().unwrap_or_else(|| &NSize {sign: false, width: 0});
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
            },
            _ => println!("Vector compilation only currently possible with primitives")
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
