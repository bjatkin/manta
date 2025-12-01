/// Parses try statements
///
/// Example: `try .Ok(i) := div(10, 20) catch { return .Err }`
pub struct TryParselet;

impl PrefixStmtParselet for TryParselet {
    fn parse(&self, parser: &mut Parser, left: Expr, _token: Token) -> Result<Stmt, ParseError> {
        todo!();
    }
}
