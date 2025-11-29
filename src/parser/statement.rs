use crate::ast::{Block, ExprStmt, Stmt};
use crate::parser::lexer::TokenKind;
use crate::parser::{ParseError, Parser};

/// Parse manta statements
pub fn parse_statement(parser: &mut Parser) -> Result<Stmt, ParseError> {
    // need to peek here because we don't know if this is an expression or a statement yet
    let token = parser.lookahead(0)?;
    let token_kind = token.kind;

    let parselet = parser.statement_parselets.get(&token_kind);
    if parselet.is_none() {
        let expr = parser.parse_expression()?;
        println!("got expression {:?}", expr);
        return Ok(Stmt::Expr(ExprStmt { expr }));
    }

    let parselet = parselet.unwrap().clone();

    let token = parser.consume()?;
    parselet.parse(parser, token)
}

pub fn parse_block(parser: &mut Parser) -> Result<Block, ParseError> {
    // let matched = parser.match_token(TokenKind::OpenBrace)?;
    // if !matched {
    //     return Err(ParseError::UnexpectedToken(
    //         "blocks must start with '{'".to_string(),
    //     ));
    // }

    let mut statements = vec![];
    loop {
        let matched = parser.match_token(TokenKind::CloseBrace)?;
        if matched {
            break;
        }

        let matches = parser.match_token(TokenKind::Eof)?;
        if matches {
            return Err(ParseError::UnexpectedToken(
                "missing closing '}' in block".to_string(),
            ));
        }

        let statement = parse_statement(parser)?;
        statements.push(statement);
    }

    Ok(Block { statements })
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ast::{
        BinaryExpr, BinaryOp, DeferStmt, Expr, FieldAccess, FreeExpr, IdentifierExpr, IndexAccess,
        NewExpr, ReturnStmt, Stmt,
    };
    use crate::ast::{FunctionCall, LetStmt, TypeSpec};
    use crate::parser::lexer::Lexer;

    macro_rules! test_parse_statement {
        ( $( $case:ident { input: $input:expr, want_var: $want_var:pat, want_value: $want_value:expr, } ),*, ) => {
            $(
                #[test]
                fn $case() {
                    let lexer = Lexer::new($input);
                    let mut parser = Parser::new(lexer);

                    let stmt = parse_statement(&mut parser).unwrap();
                    match stmt {
                        $want_var => $want_value,
                        _ => panic!("Expected {} => {}, but got {:?}", stringify!($want_var), stringify!($want_value), stmt),
                    }
                }

            )*
        }
    }

    test_parse_statement!(
        parse_stmt_let {
            input: "let x i32",
            want_var: Stmt::Let(stmt),
            want_value: assert_eq!(
                stmt,
                LetStmt {
                    name: "x".to_string(),
                    type_annotation: Some(TypeSpec::Int32),
                    initializer: None
                }
            ),
        },
        parse_stmt_let_with_value {
            input: "let y bool = true",
            want_var: Stmt::Let(stmt),
            want_value: assert_eq!(
                stmt,
                LetStmt {
                    name: "y".to_string(),
                    type_annotation: Some(TypeSpec::Bool),
                    initializer: Some(Expr::BoolLiteral(true)),
                },
            ),
        },
        parse_stmt_let_no_type {
            input: "let pi = 3.14",
            want_var: Stmt::Let(stmt),
            want_value: assert_eq!(
                stmt,
                LetStmt {
                    name: "pi".to_string(),
                    type_annotation: None,
                    initializer: Some(Expr::FloatLiteral(3.14)),
                }
            ),
        },
        parse_stmt_let_user_type {
            input: "let jill Person = new_person()",
            want_var: Stmt::Let(stmt),
            want_value: assert_eq!(
                stmt,
                LetStmt {
                    name: "jill".to_string(),
                    type_annotation: Some(TypeSpec::Named("Person".to_string())),
                    initializer: Some(Expr::Call(FunctionCall {
                        func: Box::new(Expr::Identifier(IdentifierExpr {
                            name: "new_person".to_string()
                        })),
                        args: vec![],
                    })),
                },
            ),
        },
        parse_stmt_return {
            input: "return",
            want_var: Stmt::Return(stmt),
            want_value: assert_eq!(stmt, ReturnStmt { value: None }),
        },
        parse_stmt_return_value {
            input: "return 10",
            want_var: Stmt::Return(stmt),
            want_value: assert_eq!(
                stmt,
                ReturnStmt {
                    value: Some(Expr::IntLiteral(10))
                }
            ),
        },
        parse_stmt_return_complex_value {
            input: "return builder.string(true)[0]",
            want_var: Stmt::Return(stmt),
            want_value: assert_eq!(
                stmt,
                ReturnStmt {
                    value: Some(Expr::Index(IndexAccess {
                        target: Box::new(Expr::Call(FunctionCall {
                            func: Box::new(Expr::FieldAccess(FieldAccess {
                                target: Box::new(Expr::Identifier(IdentifierExpr {
                                    name: "builder".to_string(),
                                })),
                                field: Box::new(IdentifierExpr {
                                    name: "string".to_string()
                                }),
                            })),
                            args: vec![Expr::BoolLiteral(true)],
                        })),
                        index: Box::new(Expr::IntLiteral(0)),
                    }))
                }
            ),
        },
        parse_stmt_defer {
            input: "defer {}",
            want_var: Stmt::Defer(stmt),
            want_value: assert_eq!(
                stmt,
                DeferStmt {
                    block: Block { statements: vec![] }
                }
            ),
        },
        parse_stmt_defer_free {
            input: "defer {\nfree(ptr)\n}",
            want_var: Stmt::Defer(stmt),
            want_value: assert_eq!(
                stmt,
                DeferStmt {
                    block: Block {
                        statements: vec![Stmt::Expr(ExprStmt {
                            expr: Expr::Free(FreeExpr {
                                expr: Box::new(Expr::Identifier(IdentifierExpr {
                                    name: "ptr".to_string()
                                })),
                            })
                        })]
                    }
                }
            ),
        },
        parse_stmt_defer_multi {
            input: "defer {\nprint(\"done:\", err)\nnew(i32)\n}",
            want_var: Stmt::Defer(stmt),
            want_value: assert_eq!(
                stmt,
                DeferStmt {
                    block: Block {
                        statements: vec![
                            Stmt::Expr(ExprStmt {
                                expr: Expr::Call(FunctionCall {
                                    func: Box::new(Expr::Identifier(IdentifierExpr {
                                        name: "print".to_string()
                                    })),
                                    args: vec![
                                        Expr::StringLiteral("done:".to_string()),
                                        Expr::Identifier(IdentifierExpr {
                                            name: "err".to_string(),
                                        })
                                    ],
                                })
                            }),
                            Stmt::Expr(ExprStmt {
                                expr: Expr::New(NewExpr {
                                    type_spec: TypeSpec::Int32,
                                    len: None,
                                    cap: None,
                                })
                            })
                        ]
                    }
                }
            ),
        },
        parse_stmt_block {
            input: "{\nlet a = 10\nlet b i16 = 20\nlet c = a + b\n}",
            want_var: Stmt::Block(stmt),
            want_value: assert_eq!(
                stmt,
                Block {
                    statements: vec![
                        Stmt::Let(LetStmt {
                            name: "a".to_string(),
                            type_annotation: None,
                            initializer: Some(Expr::IntLiteral(10)),
                        }),
                        Stmt::Let(LetStmt {
                            name: "b".to_string(),
                            type_annotation: Some(TypeSpec::Int16),
                            initializer: Some(Expr::IntLiteral(20)),
                        }),
                        Stmt::Let(LetStmt {
                            name: "c".to_string(),
                            type_annotation: None,
                            initializer: Some(Expr::BinaryExpr(BinaryExpr {
                                left: Box::new(Expr::Identifier(IdentifierExpr {
                                    name: "a".to_string()
                                })),
                                operator: BinaryOp::Add,
                                right: Box::new(Expr::Identifier(IdentifierExpr {
                                    name: "b".to_string()
                                })),
                            })),
                        }),
                    ]
                }
            ),
        },
    );
}
