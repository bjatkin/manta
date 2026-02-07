pub mod declaration;
pub mod expression;
pub mod lexer;
pub mod pattern;
pub mod statement;
pub mod types;

use crate::ast::{Decl, Module};
use crate::str_store::{self, StrStore};

use declaration::DeclParser;
use lexer::{Lexer, Token, TokenKind};
use serde::{Deserialize, Serialize};

/// Parse error type for the parser core.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ParseError {
    Custom(Token, String),
    UnexpectedToken(Token, String),
    MissingExpression(Token, String),
    InvalidTypeSpec(Token, String),
    InvalidArguments(Token, String),
    InvalidExpression(Token, String),
}

/// A minimal Parser core scaffolding. This implements a buffered token stream
/// with lookahead and simple parselet registration. The parselet registries
/// are intentionally simple (Vec-based) to avoid requiring `TokenKind: Hash`.
pub struct Parser {
    source: String,
    decl_parser: DeclParser,
}

impl Parser {
    /// Create a new parser for a piece of source code
    pub fn new(source: String) -> Self {
        Parser {
            source,
            decl_parser: DeclParser::new(),
        }
    }

    /// Parse a Manta module
    pub fn parse_module(&self, str_store: &mut StrStore) -> Module {
        let mut lexer = Lexer::new(&self.source, str_store);

        let mut declarations = vec![];
        let mut errors = vec![];
        loop {
            let token_kind = lexer.peek().kind;
            if token_kind == TokenKind::Eof {
                break;
            }
            // TODO: the parse method should return both the Decl as well as a vector of errors. We
            // want to try and compile no matter what so having some of the declaration info is
            // important and for decls like a function its possible to have way more than a single
            // ParseError that needs to be reported
            let decl = match self.decl_parser.parse(&mut lexer) {
                Ok(decl) => decl,
                Err(err) => {
                    errors.push(err);
                    Decl::Invalid
                }
            };
            declarations.push(decl);
        }

        self.check_module(errors, declarations)
    }

    fn check_module(&self, mut errors: Vec<ParseError>, declarations: Vec<Decl>) -> Module {
        if declarations.is_empty() {
            errors.push(ParseError::Custom(
                Token {
                    kind: TokenKind::Identifier,
                    source_id: 0,
                    lexeme_id: 0,
                },
                "module is empty".to_string(),
            ));
        };

        let mut name = str_store::NIL;
        let mut modules = vec![];
        for (i, decl) in declarations.iter().enumerate() {
            match decl {
                Decl::Mod(module) => {
                    if i == 0 {
                        name = module.name;
                    } else {
                        errors.push(ParseError::Custom(
                            Token {
                                kind: TokenKind::Identifier,
                                source_id: 0,
                                lexeme_id: 0,
                            },
                            "only a single module name is allowed per file".to_string(),
                        ));
                    }
                }
                Decl::Use(using) => {
                    if i == 0 {
                        errors.push(ParseError::Custom(
                            Token {
                                kind: TokenKind::Identifier,
                                source_id: 0,
                                lexeme_id: 0,
                            },
                            "first declaration in a file must be the module name".to_string(),
                        ));
                    } else if i == 1 {
                        modules = using.modules.clone();
                    } else {
                        errors.push(ParseError::Custom(
                        Token {
                            kind: TokenKind::Identifier,
                            source_id: 0,
                            lexeme_id: 0,
                        },
                        "only a single import section allowed per file, and it must be right below the module name".to_string(),
                    ));
                    }
                }
                _ => {
                    if i == 0 {
                        errors.push(ParseError::Custom(
                            Token {
                                kind: TokenKind::Identifier,
                                source_id: 0,
                                lexeme_id: 0,
                            },
                            "first declaration in a file must be the module name".to_string(),
                        ));
                    }
                }
            }
        }

        Module::new(name, modules, errors, declarations)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;
    use std::fs;
    use std::path::Path;

    #[test]
    fn parse_file_tests() {
        let test_dir = Path::new("tests/src");
        let parser_dir = Path::new("tests/parser");

        if !test_dir.exists() {
            panic!(
                "Test directory does not exist. Please create a '{:?}' with test .manta files.",
                test_dir
            );
        }

        let entries = fs::read_dir(test_dir).expect("Failed to read tests/parser directory");

        // Read all .manta files from tests/parser and check them against expected parser output
        for entry in entries {
            assert_file_eq(entry, test_dir, parser_dir);
        }
    }

    fn assert_file_eq(
        entry: Result<std::fs::DirEntry, std::io::Error>,
        test_dir: &std::path::Path,
        parser_dir: &std::path::Path,
    ) {
        let entry = match entry {
            Ok(dir) => dir,
            Err(_) => panic!("Failed to read entry in '{:?}' directory", test_dir),
        };

        let path = entry.path();
        let ext = path.extension().expect("Failed to get file extension");
        if ext != "manta" {
            // Skip over non-manta files
            return;
        }

        let file_name = path
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("unknown");

        let source = match fs::read_to_string(&path) {
            Ok(s) => s,
            Err(_) => format!("Failed to read {}", path.display()),
        };

        let mut str_store = StrStore::new();
        let parser = Parser::new(source);
        let ast = parser.parse_module(&mut str_store);

        if !ast.get_errors().is_empty() {
            panic!("Parser error for {}: {:?}", file_name, ast.get_errors());
        };

        let json_output =
            serde_json::to_string_pretty(&ast).expect("Failed to serialize AST to JSON");

        let parser_file = parser_dir.join(format!("{}.json", file_name));

        if parser_file.exists() {
            let expected_json = match fs::read_to_string(&parser_file) {
                Ok(s) => s,
                Err(_) => format!("Failed to read {}", parser_file.display()),
            };

            assert_eq!(
                json_output, expected_json,
                "Parser output mismatch for {}",
                file_name
            );
        } else {
            // Create the parser directory if it doesn't exist
            fs::create_dir_all(parser_dir).expect("Failed to create parser test directory");

            // Write the output if the file does not exist
            match fs::write(&parser_file, &json_output) {
                Ok(_) => (),
                Err(_) => panic!("Failed to write parser output to {:?}", parser_file),
            };

            // If we generated the output file, fail the test to prompt the user to verify it's correctness
            panic!(
                "Generated new parser output file: {:?}. Please verify its correctness.",
                parser_file
            );
        }
    }
}
