use crate::ast::Expr;
use crate::parser::lexer::Token;
use crate::parser::parselets::PrefixParselet;
use crate::parser::{ParseError, Parser};

/// Parses nil literal expressions.
///
/// Example: `nil`
pub struct NilLiteralParselet;

impl PrefixParselet for NilLiteralParselet {
    fn parse(&self, _parser: &mut Parser, _token: Token) -> Result<Expr, ParseError> {
        Ok(Expr::NilLiteral)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::lexer::{Span, TokenKind};

    #[test]
    fn test_parse_nil() {
        let token = Token::new(TokenKind::NilLiteral, None, Span::new(0, 3));
        let mut parser = Parser::new(crate::parser::lexer::Lexer::new(""));
        let result = NilLiteralParselet.parse(&mut parser, token);
        assert!(result.is_ok());
        match result.unwrap() {
            Expr::NilLiteral => (),
            _ => panic!("Expected NilLiteral"),
        }
    }
}
