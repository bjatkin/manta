use crate::ast::Expr;
use crate::parser::lexer::Token;
use crate::parser::parselets::PrefixParselet;
use crate::parser::{ParseError, Parser};

/// Parses identifier expressions.
///
/// Example: `foo`, `myVariable`, `count`
pub struct IdentifierParselet;

impl PrefixParselet for IdentifierParselet {
    fn parse(&self, _parser: &mut Parser, token: Token) -> Result<Expr, ParseError> {
        let lexeme = token
            .lexeme
            .ok_or_else(|| ParseError::Custom("Identifier missing lexeme".to_string()))?;

        Ok(Expr::Identifier(lexeme))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::lexer::{Lexer, Span, TokenKind};

    struct TestCase {
        token: Token,
        expected: &'static str,
    }
    #[test]
    fn test_parse_identifiers() {
        let tests = vec![
            TestCase {
                token: Token::new(TokenKind::Ident, Some("foo".to_string()), Span::new(0, 3)),
                expected: "foo",
            },
            TestCase {
                token: Token::new(
                    TokenKind::Ident,
                    Some("my_var".to_string()),
                    Span::new(0, 6),
                ),
                expected: "my_var",
            },
            TestCase {
                token: Token::new(
                    TokenKind::Ident,
                    Some("var123".to_string()),
                    Span::new(0, 6),
                ),
                expected: "var123",
            },
        ];

        for test in tests {
            let mut parser = Parser::new(Lexer::new(""));
            let result = IdentifierParselet.parse(&mut parser, test.token);
            assert!(result.is_ok());

            match result.unwrap() {
                Expr::Identifier(name) => assert_eq!(name, test.expected),
                _ => panic!("Expected Identifier"),
            }
        }
    }

    #[test]
    fn test_missing_lexeme() {
        let token = Token::new(TokenKind::Ident, None, Span::new(0, 0));
        let mut parser = Parser::new(Lexer::new(""));
        let result = IdentifierParselet.parse(&mut parser, token);
        assert!(result.is_err());
    }
}
