// ratio
#![allow(mixed_script_confusables)]

mod ast;
mod parse;
mod emit;
mod types;

use ariadne::{sources, Color, ColorGenerator, Label, Report, ReportKind};
use chumsky::prelude::*;
use calyx_ir as ir;
use std::io::Write;

use ast::*;
use parse::*;

pub type Span = SimpleSpan<usize>;

pub type Spanned<T> = (T, Span);

pub type ParserInput<'tokens, 'src> =
    chumsky::input::SpannedInput<Token<'src>, Span, &'tokens [(Token<'src>, Span)]>;

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

        if let Some(e) = ast.clone() {
            let tc_err = types::check_all(&vec![Decl { id: "main".to_owned(), expr: e.0, ty: (Type::Num(NType{sign: false, width: 8}), SimpleSpan::new(0, 0)) }]);

            let mut report = Report::build(ReportKind::Error, filename.clone(), e.1.start);

            let tc_err = tc_err.unwrap_err();

            let mut bt = vec![tc_err];

            let mut cg = ColorGenerator::from_state([30000, 15000, 35000], 0.2);

            let mut order = 1000;

            while let Some(err) = bt.pop() {

                match err {
                    types::Error::MismatchedTypeDecl { expected, actual, expr } => {
                        report = report.with_message("Type didn't match declaration!")
                            .with_label(
                                Label::new((filename.clone(), expr.1.into_range()))
                                    .with_message(format!("This expression had type {}", actual))
                                    .with_color(Color::Red)
                            )
                            .with_label(
                                Label::new((filename.clone(), expected.1.into_range()))
                                    .with_message("But expected this type")
                                    .with_color(Color::Blue)
                                    .with_priority(100)
                                    .with_order(1)
                            );
                    }
                    types::Error::MismatchedType { expected, actual, loc } => {
                        report = report.with_message("Expected type {}, but found type {}")
                            .with_label(
                                Label::new((filename.clone(), loc.into_range()))
                                    .with_message("for this expression")
                                    .with_color(Color::Red)
                             );
                    }
                    types::Error::IdentifierNotFound((id, loc)) => {
                        report = report.with_message(format!("Variable {} is not in scope", id))
                            .with_label(
                                Label::new((filename.clone(), loc.into_range()))
                                    .with_message("Used here")
                                    .with_color(Color::Red)
                            );
                    }
                    types::Error::InvalidVectorSize { loc, count } => {
                        report = report.with_message("Invalid vector size!")
                            .with_label(
                                Label::new((filename.clone(), loc.into_range()))
                                .with_message(format!("This vector had size {count}"))
                            )
                            .with_note("Vector sizes must be a power of 2");
                    }
                    types::Error::WithContext { ctx, err } => {
                        match ctx {
                            types::ContextInfo::WhileApplying(_, _, _) => todo!(),
                            types::ContextInfo::WhileChecking(expr, ty) => {
                                report = report.with_label(
                                    Label::new((filename.clone(), expr.1.into_range()))
                                        .with_message(format!("while checking expression had type {}", ty.0))
                                        .with_color(cg.next())
                                        .with_order(order)
                                );

                                order -= 1;
                            }
                            types::ContextInfo::WhileUnifying(_, _) => todo!(),
                            types::ContextInfo::InExpression(s) => {
                                report = 
                                    report.with_label(
                                        Label::new((filename.clone(), s.into_range()))
                                            .with_message("while checking expression")
                                            .with_color(cg.next())
                                            .with_order(order)
                                );

                                order -= 1;
                            }
                        }
                        bt.push(*err);
                    }
                    types::Error::NonNumericType(t) => {
                        report = report.with_message(format!("Found unexpected non-numeric type {:?}", t));
                    }
                    types::Error::UnableToUnify { t1, t2 } => {
                        report = report.with_message(format!("Unable to unify types {} and {}", t1, t2));
                    }
                }
            }

            report.finish().print(sources([(filename.clone(), &src)])).expect("stdout io err");
        };


        // let ctx = match emit::emit(ast) {
        //     Ok(v) => v,
        //     Err(e) => {
        //         let (file, start, end) = {
        //             let (file, start, end) = e.location();

        //             (file.to_owned(), start, end)

        //         };

        //         Report::build(ReportKind::Error, file.clone(), start)
        //             .with_message(e.message())
        //             .with_label(
        //                 Label::new((file.clone(), start..end))
        //                     .with_message(e.message())
        //                     .with_color(Color::Red)
        //             )
        //             .finish()
        //             .print(sources([(file.clone(), &src)]))
        //             .expect("stdout io err");

        //         return Err(std::io::Error::new(std::io::ErrorKind::Other, "oh no!"));
        //     }
        // };

        // let out = &mut std::io::stdout();

        // for comp in &ctx.components {
        //     ir::Printer::write_component(comp, out).expect("stdout io err");
        //     writeln!(out)?;
        // }

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
