use crate::ast::{AssignStmt, Expr, Stmt};
use crate::parser::lexer::Token;
use crate::parser::parselets::InfixStmtParselet;
use crate::parser::{ParseError, Parser};

/// Parses assignment statements
///
/// Example: `x = 10`
pub struct AssignParselet;

impl InfixStmtParselet for AssignParselet {
    fn parse(&self, parser: &mut Parser, left: Expr, _token: Token) -> Result<Stmt, ParseError> {
        let expr = parser.parse_expression()?;

        Ok(Stmt::Assign(AssignStmt {
            lvalue: left,
            rvalue: expr,
        }))
    }
}
