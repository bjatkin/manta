use crate::ast::{Expr, Stmt, TypeSpec};
use crate::checker::expression::ExprChecker;
use crate::checker::sym_table::SymTable;
use crate::checker::{CheckerError, ErrorStore};

pub struct StmtChecker {
    expr_checker: ExprChecker,
}

impl StmtChecker {
    pub fn new() -> Self {
        StmtChecker {
            expr_checker: ExprChecker {},
        }
    }

    pub fn check(
        &mut self,
        error_store: &mut ErrorStore,
        sym_table: &mut SymTable,
        stmts: Vec<Stmt>,
    ) {
        for stmt in stmts {
            self.check_stmt(error_store, sym_table, stmt)
        }
    }

    fn check_stmt(&mut self, error_store: &mut ErrorStore, sym_table: &mut SymTable, stmt: Stmt) {
        match stmt {
            Stmt::Let(stmt) => {
                todo!("this is actually pretty trick because let is pretty powerful")
            }
            Stmt::Assign(stmt) => {
                let r_type_spec = self.expr_checker.check(error_store, sym_table, stmt.rvalue);

                match stmt.lvalue {
                    Expr::Identifier(expr) => {
                        let ident_name = expr.name;
                        let binding = sym_table.get_binding(&ident_name);
                        match binding {
                            Some(b) => {
                                if b.type_spec != r_type_spec {
                                    error_store.push(
                                        format!("can not assign expression with type {:?} to identifier {:?}", r_type_spec, ident_name)
                                        );
                                }
                            }
                            None => {
                                error_store.push(format!("identifier {} is not known", ident_name));
                            }
                        }
                    }
                    Expr::Index(expr) => {
                        let index_type =
                            self.expr_checker.check(error_store, sym_table, *expr.index);

                        let target_type =
                            self.expr_checker
                                .check(error_store, sym_table, *expr.target);
                        match target_type {
                            TypeSpec::Slice(target_type) => {}
                            TypeSpec::Array(target_type) => {}
                            _ => error_store.push("can only index slices or arrays".to_string()),
                        }
                    }
                    Expr::DotAccess(expr) => {
                        todo!("need to support assigning to dot access")
                    }
                    _ => todo!("support assigning to other expression types"),
                }
            }
            Stmt::Expr(stmt) => {
                self.expr_checker.check(error_store, sym_table, stmt.expr);
            }
            Stmt::Return(stmt) => {
                // TODO: get the type of the expression (stmt.value)

                match sym_table.get_return_type() {
                    Some(t) => {
                        if stmt.value.is_none() {
                            panic!("missing return!")
                        }

                        let type_spec =
                            self.expr_checker
                                .check(error_store, sym_table, stmt.value.unwrap());

                        if t != type_spec {
                            error_store.push(format!("{:?} does not match {:?}", t, type_spec));
                        }
                    }
                    None => {
                        if stmt.value.is_some() {
                            panic!("no return value expected")
                        }
                    }
                }
            }
            Stmt::Defer(stmt) => self.check(error_store, sym_table, stmt.block.statements),
            Stmt::Match(stmt) => todo!("need to support this"),
            Stmt::Block(stmt) => self.check(error_store, sym_table, stmt.statements),
            Stmt::If(stmt) => todo!("need to support this"),
        }
    }
}
