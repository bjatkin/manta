use super::Precedence;
use crate::ast::{Expr, FunctionCall};
use crate::parser::lexer::{Token, TokenKind};
use crate::parser::parselets::InfixParselet;
use crate::parser::{ParseError, Parser};

/// Parses function call expressions.
///
//// Example: `foo(1, 2, "bar")`
pub struct CallParselet;

impl InfixParselet for CallParselet {
    fn parse(&self, parser: &mut Parser, left: Expr, _token: Token) -> Result<Expr, ParseError> {
        let mut arguments = Vec::new();

        // Check for empty argument list
        let matched = parser.match_token(TokenKind::CloseParen)?;
        if matched {
            return Ok(Expr::Call(crate::ast::FunctionCall {
                func: Box::new(left),
                args: arguments,
            }));
        }

        // Parse arguments list
        loop {
            let arg = parser.parse_expression()?;
            arguments.push(arg);

            let matched = parser.match_token(TokenKind::Comma)?;
            if !matched {
                break;
            }
        }

        // Expect a closing ')'
        let matched = parser.match_token(TokenKind::CloseParen)?;
        if !matched {
            return Err(ParseError::UnexpectedToken(
                "expected ')' after function arguments".to_string(),
            ));
        }

        Ok(Expr::Call(FunctionCall {
            func: Box::new(left),
            args: arguments,
        }))
    }

    fn precedence(&self) -> super::Precedence {
        Precedence::Call
    }
}
