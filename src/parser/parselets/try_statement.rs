use crate::ast::{Expr, Stmt};
use crate::parser::lexer::Token;
use crate::parser::parselets::PrefixStmtParselet;
use crate::parser::{ParseError, Parser};

/// Parses try statements
///
/// Example: `try .Ok(i) := div(10, 20) catch { return .Err }`
pub struct TryParselet;

impl PrefixStmtParselet for TryParselet {
    fn parse(&self, parser: &mut Parser, _token: Token) -> Result<Stmt, ParseError> {
        todo!();
    }
}
