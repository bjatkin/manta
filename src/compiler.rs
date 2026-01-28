use crate::checker::Checker;
use crate::parser::ParseError;
use crate::parser::Parser;

pub struct Compiler {}

impl Compiler {
    pub fn new() -> Self {
        Compiler {}
    }

    pub fn compile(&self, source: String) -> Result<(), ParseError> {
        let parser = Parser::new(source);
        let program = parser.parse_program()?;

        let mut checker = Checker::new();
        checker.check(program);

        Ok(())
    }
}
