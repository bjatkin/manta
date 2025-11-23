use crate::ast::Expr;
use crate::parser::lexer::Token;
use crate::parser::parselets::PrefixParselet;
use crate::parser::{ParseError, Parser};

/// Parses floating-point literal expressions.
///
/// Example: `3.14`
pub struct FloatLiteralParselet;

impl PrefixParselet for FloatLiteralParselet {
    fn parse(&self, _parser: &mut Parser, token: Token) -> Result<Expr, ParseError> {
        let lexeme = token
            .lexeme
            .ok_or_else(|| ParseError::Custom("Float literal missing lexeme".to_string()))?;

        let value = lexeme
            .parse::<f64>()
            .map_err(|_| ParseError::invalid_float(&lexeme))?;

        Ok(Expr::FloatLiteral(value))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::lexer::{Lexer, Span, TokenKind};

    struct TestCase {
        token: Token,
        expected: f64,
    }

    #[test]
    fn test_parse_float() {
        let tests = vec![
            TestCase {
                token: Token::new(TokenKind::Float, Some("3.14".to_string()), Span::new(0, 4)),
                expected: 3.14,
            },
            TestCase {
                token: Token::new(TokenKind::Float, Some("0.0".to_string()), Span::new(0, 3)),
                expected: 0.0,
            },
            TestCase {
                token: Token::new(
                    TokenKind::Float,
                    Some("1.23e4".to_string()),
                    Span::new(0, 6),
                ),
                expected: 12300.0,
            },
        ];

        for test in tests {
            let mut parser = Parser::new(Lexer::new(""));
            let result = FloatLiteralParselet.parse(&mut parser, test.token);
            assert!(result.is_ok());

            match result.unwrap() {
                Expr::FloatLiteral(f) => {
                    assert!((f - test.expected).abs() < f64::EPSILON)
                }
                _ => panic!("Expected FloatLiteral(0.0)"),
            }
        }
    }

    #[test]
    fn test_missing_lexeme() {
        let token = Token::new(TokenKind::Float, None, Span::new(0, 0));
        let mut parser = Parser::new(Lexer::new(""));
        let result = FloatLiteralParselet.parse(&mut parser, token);
        assert!(result.is_err());
    }
}
