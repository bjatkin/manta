use crate::ast::{Expr, ShortLetStmt, Stmt};
use crate::parser::lexer::Token;
use crate::parser::parselets::InfixStmtParselet;
use crate::parser::{ParseError, Parser};

/// Parses short let statments
///
/// Example: `x := 10`
pub struct ShortLetParselet;

impl InfixStmtParselet for ShortLetParselet {
    fn parse(&self, parser: &mut Parser, left: Expr, _token: Token) -> Result<Stmt, ParseError> {
        let expr = parser.parse_expression()?;
        match left {
            Expr::Identifier(e) => Ok(Stmt::ShortLet(ShortLetStmt {
                name: e,
                value: expr,
            })),
            _ => Err(ParseError::InvalidExpression(
                "variable declarations must have a identifier on the left hand side".to_string(),
            )),
        }
    }
}
