use crate::ast::Expr;
use crate::parser::lexer::Token;
use crate::parser::parselets::PrefixParselet;
use crate::parser::{ParseError, Parser};

/// Parses string literal expressions.
///
/// Example: `"hello world"`
pub struct StringLiteralParselet;

impl PrefixParselet for StringLiteralParselet {
    fn parse(&self, _parser: &mut Parser, token: Token) -> Result<Expr, ParseError> {
        let lexeme = token
            .lexeme
            .ok_or_else(|| ParseError::Custom("String literal missing lexeme".to_string()))?;

        Ok(Expr::StringLiteral(lexeme))
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
    fn test_parse_simple_string() {
        let tests = vec![
            TestCase {
                token: Token::new(TokenKind::Str, Some("hello".to_string()), Span::new(0, 7)),
                expected: "hello",
            },
            TestCase {
                token: Token::new(TokenKind::Str, Some("".to_string()), Span::new(0, 2)),
                expected: "",
            },
            TestCase {
                token: Token::new(
                    TokenKind::Str,
                    Some("hello world".to_string()),
                    Span::new(0, 13),
                ),
                expected: "hello world",
            },
        ];

        for test in tests {
            let mut parser = Parser::new(Lexer::new(""));
            let result = StringLiteralParselet.parse(&mut parser, test.token);
            assert!(result.is_ok());

            match result.unwrap() {
                Expr::StringLiteral(s) => assert_eq!(s, test.expected),
                _ => panic!("Expected StringLiteral"),
            }
        }
    }

    #[test]
    fn test_missing_lexeme() {
        let token = Token::new(TokenKind::Str, None, Span::new(0, 0));
        let mut parser = Parser::new(Lexer::new(""));
        let result = StringLiteralParselet.parse(&mut parser, token);
        assert!(result.is_err());
    }
}
