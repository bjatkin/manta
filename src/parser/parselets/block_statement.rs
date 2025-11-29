use crate::ast::Stmt;
use crate::parser::lexer::Token;
use crate::parser::parselets::StatementParselet;
use crate::parser::statement;
use crate::parser::{ParseError, Parser};

/// Parses a block statement.
///
/// Example `{ print("ok"); free(ptr) }`
pub struct BlockParselet;

impl StatementParselet for BlockParselet {
    fn parse(&self, parser: &mut Parser, _token: Token) -> Result<Stmt, ParseError> {
        println!("inside block parselet");
        let block = statement::parse_block(parser)?;

        Ok(Stmt::Block(block))
    }
}
