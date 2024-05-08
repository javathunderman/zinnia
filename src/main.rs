// ratio
#![allow(mixed_script_confusables)]

mod ast;
mod emit;
mod parse;
mod types;

use ariadne::{sources, Color, ColorGenerator, Label, Report, ReportKind};
use calyx_ir as ir;
use chumsky::prelude::*;
use std::{io::Write, process::exit};
use std::fs::File;

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
        let (decls, parse_errs) = decl_parser()
            .map_with(|ast, e| (ast, e.span()))
            .parse(tokens.as_slice().spanned((src.len()..src.len()).into()))
            .into_output_errors();

        if let Some((decls, loc)) = decls {
            let tc = types::check_all(&decls);

            let _res = match tc {
                // TODO: typed ast
                Ok(o) => o,
                Err(err) => {
                    report_tc_error(&filename, loc.start, err, &src);

                    // TODO: into io::Error
                    exit(-1)
                }
            };

            let main = decls
                .iter()
                .find(|x| x.id.0 == "main")
                .expect("Missing a main!");

            let ctx = match emit::emit(&main.expr) {
                Ok(v) => {
                    let mut calyx_out = File::create("out/output.futil")?;
                    let mut json_out = File::create("out/data.json")?;
                    write!(calyx_out, "import \"primitives/core.futil\";\nimport \"primitives/memories/comb.futil\";");

                    for c in &v.0.components {
                        ir::Printer::write_component(&c, &mut calyx_out);
                    }
                    write!(json_out, "{}", v.1.to_string());
                    v.0
                },
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
                                .with_color(Color::Red),
                        )
                        .finish()
                        .print(sources([(file.clone(), &src)]))
                        .expect("stdout io err");

                    return Err(std::io::Error::new(std::io::ErrorKind::Other, "oh no!"));
                }
            };
        };

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

fn report_tc_error(filename: &String, start: usize, tc_err: types::Error, src: &String) {
    let mut report = Report::build(ReportKind::Error, filename.clone(), start);
    let mut order = 1000;

    let mut bt = vec![tc_err];

    let mut cg = ColorGenerator::from_state([30000, 15000, 35000], 0.05);

    while let Some(err) = bt.pop() {
        match err {
            types::Error::MismatchedTypeDecl {
                expected,
                actual,
                expr,
            } => {
                report = report
                    .with_message("Type didn't match declaration!")
                    .with_label(
                        Label::new((filename.clone(), expr.1.into_range()))
                            .with_message(format!("This expression had type {}", actual))
                            .with_color(Color::Red),
                    )
                    .with_label(
                        Label::new((filename.clone(), expected.1.into_range()))
                            .with_message("But expected this type")
                            .with_color(Color::Blue)
                            .with_priority(100)
                            .with_order(1),
                    );
            }
            types::Error::MismatchedType {
                expected,
                actual,
                loc,
            } => {
                report = report
                    .with_message(format!("Expected type {expected}, but found type {actual}"))
                    .with_label(
                        Label::new((filename.clone(), loc.into_range()))
                            .with_message("for this expression")
                            .with_color(Color::Red),
                    );
            }
            types::Error::IdentifierNotFound((id, loc)) => {
                report = report
                    .with_message(format!("Variable {} is not in scope", id))
                    .with_label(
                        Label::new((filename.clone(), loc.into_range()))
                            .with_message("Used here")
                            .with_color(Color::Red),
                    );
            }
            types::Error::InvalidVectorSize { loc, count } => {
                report = report
                    .with_message("Invalid vector size!")
                    .with_label(
                        Label::new((filename.clone(), loc.into_range()))
                            .with_message(format!("This vector had size {count}")),
                    )
                    .with_note("Vector sizes must be a power of 2");
            }
            types::Error::WithContext { ctx, err } => {
                match ctx {
                    types::ContextInfo::WhileApplying(_, _, _) => todo!(),
                    types::ContextInfo::WhileChecking(span, ty) => {
                        report = report.with_label(
                            Label::new((filename.clone(), span.into_range()))
                                .with_message(format!(
                                    "while checking expression had type {}",
                                    ty.0
                                ))
                                .with_color(cg.next())
                                .with_order(order),
                        );

                        order -= 1;
                    }
                    types::ContextInfo::WhileUnifying(t1, t2) => {
                        report = report.with_note(format!("while unifying {t1} and {t2}"));
                        order -= 1;
                    }
                    types::ContextInfo::InExpression(s) => {
                        report = report.with_label(
                            Label::new((filename.clone(), s.into_range()))
                                .with_message("while checking expression")
                                .with_color(cg.next())
                                .with_order(order),
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
            types::Error::ReservedIdentifier { id, loc } => {
                report = report
                    .with_message(format!("Use of reserved identifier {id}"))
                    .with_label(
                        Label::new((filename.clone(), loc.into_range()))
                            .with_message("variable declared here")
                            .with_color(Color::Red),
                    );
            }
            types::Error::MismatchedOperands { expr, t_lhs, l_lhs, t_rhs, l_rhs  } => {
                report = report.with_message("Mismatched operand types!")
                    .with_label(Label::new((filename.clone(), expr.start()+1..expr.start()+2))
                        .with_color(Color::Yellow))
                    .with_label(Label::new((filename.clone(), l_lhs.into_range()))
                        .with_message(format!("this had type {t_lhs}"))
                        .with_color(Color::Blue))
                    .with_label(Label::new((filename.clone(), l_rhs.into_range()))
                        .with_message(format!("while this had type {t_rhs}"))
                        .with_color(Color::Green));
            }
        }
    }

    report
        .finish()
        .print(sources([(filename.clone(), src)]))
        .expect("stdout io err");
}
