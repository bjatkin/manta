use crate::ast::Expr;
use crate::parser::lexer::Token;
use crate::parser::parselets::PrefixParselet;
use crate::parser::{ParseError, Parser};

/// Parses boolean literal expressions.
///
/// Example: `true`, `false`
pub struct BoolLiteralParselet;

impl PrefixParselet for BoolLiteralParselet {
    fn parse(&self, _parser: &mut Parser, token: Token) -> Result<Expr, ParseError> {
        let value = match token.kind {
            crate::parser::lexer::TokenKind::TrueLiteral => true,
            crate::parser::lexer::TokenKind::FalseLiteral => false,
            _ => return Err(ParseError::Custom("Invalid boolean token".to_string())),
        };

        Ok(Expr::BoolLiteral(value))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::lexer::{Lexer, Span, TokenKind};

    #[test]
    fn test_parse_true() {
        let token = Token::new(TokenKind::TrueLiteral, None, Span::new(0, 4));
        let mut parser = Parser::new(Lexer::new(""));
        let result = BoolLiteralParselet.parse(&mut parser, token);
        assert!(result.is_ok());

        match result.unwrap() {
            Expr::BoolLiteral(true) => (),
            _ => panic!("Expected BoolLiteral(true)"),
        }
    }

    #[test]
    fn test_parse_false() {
        let token = Token::new(TokenKind::FalseLiteral, None, Span::new(0, 5));
        let mut parser = Parser::new(Lexer::new(""));
        let result = BoolLiteralParselet.parse(&mut parser, token);
        assert!(result.is_ok());

        match result.unwrap() {
            Expr::BoolLiteral(false) => (),
            _ => panic!("Expected BoolLiteral(false)"),
        }
    }
}
