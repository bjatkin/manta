mod binder;
mod expression;
mod statement;
mod sym_table;

use crate::ast::Decl;

use expression::ExprChecker;
use statement::StmtChecker;
use sym_table::{Binding, SymTable, Type};

// TODO: track the span so we know where the error occured
pub struct CheckerError {
    msg: String,
}

impl CheckerError {
    fn new(msg: String) -> Self {
        CheckerError { msg }
    }
}

pub struct ErrorStore {
    errors: Vec<CheckerError>,
}

impl ErrorStore {
    fn new() -> Self {
        ErrorStore { errors: vec![] }
    }

    fn push(&mut self, msg: String) {
        self.errors.push(CheckerError::new(msg.to_string()))
    }
}

pub struct Checker {
    module: String,
    sym_table: SymTable,
    error_store: ErrorStore,
    stmt_checker: StmtChecker,
    expr_checker: ExprChecker,
}

impl Checker {
    pub fn new() -> Self {
        Checker {
            module: "".to_string(),
            sym_table: SymTable::new(),
            error_store: ErrorStore::new(),
            stmt_checker: StmtChecker::new(),
            expr_checker: ExprChecker {},
        }
    }

    pub fn check(&mut self, program: Vec<Decl>) {
        if program.is_empty() {
            return;
        }

        // TODO: I'd like to do this with an enummeration and just check for
        // i to be == 0, however that results in some borrow checking issues
        // so this works better for now.
        let mut first = true;
        for decl in program {
            if first {
                if let Decl::Mod(decl) = decl {
                    self.module = decl.name.clone();
                } else {
                    self.error_store
                        .push("first decl must be a module".to_string());
                }
            } else {
                self.check_decl(decl);
            }

            first = false;
        }
    }

    fn check_decl(&mut self, decl: Decl) {
        match decl {
            Decl::Const(decl) => {
                let type_spec =
                    self.expr_checker
                        .check(&mut self.error_store, &mut self.sym_table, decl.value);
                self.sym_table.add_binding(Binding {
                    name: decl.name,
                    type_spec,
                })
            }
            Decl::Var(decl) => {
                let type_spec =
                    self.expr_checker
                        .check(&mut self.error_store, &mut self.sym_table, decl.value);
                self.sym_table.add_binding(Binding {
                    name: decl.name,
                    type_spec,
                })
            }
            Decl::Type(decl) => self.sym_table.add_type(Type {
                name: decl.name.name,
                type_spec: decl.type_spec,
            }),
            Decl::Mod(_) => self
                .error_store
                .push("only one module declaration is allowed".to_string()),
            // TODO: we should parse the use block first to resovle the DAG for module parsing
            // Then we can parse from leaves to roots so all types can be fully resolved
            Decl::Use(_) => { /* need to import modules here leave as a no-op for now*/ }
            Decl::Function(decl) => {
                self.sym_table.push_scope();

                for param in decl.params {
                    self.sym_table.add_binding(Binding {
                        name: param.name,
                        type_spec: param.type_spec,
                    });
                }

                self.sym_table.set_return_type(decl.return_type);

                self.stmt_checker.check(
                    &mut self.error_store,
                    &mut self.sym_table,
                    decl.body.statements,
                );

                self.sym_table.pop_scope();
            }
        }
    }
}
