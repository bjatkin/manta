use crate::ast::{ConstDecl, Decl, Tree};
use crate::parser::ParseError;
use crate::parser::declaration::{DeclParselet, DeclParser};
use crate::parser::lexer::{Lexer, Token, TokenKind};
use crate::store::ID;

/// Parses top-level const declarations
///
/// Example: `const PI = 3.14159`
pub struct ConstDeclParselet;

impl DeclParselet for ConstDeclParselet {
    fn parse(
        &self,
        parser: &DeclParser,
        lexer: &mut Lexer,
        tree: &mut Tree,
        _token: Token,
    ) -> ID<Decl> {
        let token = lexer.next_token();
        if token.kind != TokenKind::Identifier {
            tree.add_error(ParseError::UnexpectedToken(
                token,
                "Expected const name".to_string(),
            ));
            return tree.add_decl(Decl::Invalid);
        }

        let name = token.lexeme_id;

        // Expect '='
        let token = lexer.next_token();
        if token.kind != TokenKind::Equal {
            tree.add_error(ParseError::UnexpectedToken(
                token,
                "Expected '=' after const name".to_string(),
            ));
            return tree.add_decl(Decl::Invalid);
        }

        let value = parser.parse_expression(lexer, tree);

        tree.add_decl(Decl::Const(ConstDecl { name, value }))
    }
}
