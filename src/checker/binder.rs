use crate::ast::{Decl, Expr, Stmt, TypeSpec};
use crate::checker::ErrorStore;
use crate::checker::sym_table::{Binding, SymTable};

pub fn bind_symboles(sym_table: &mut SymTable, decls: Vec<Decl>) -> ErrorStore {
    let mut error_store = ErrorStore::new();

    for (i, decl) in decls.iter().enumerate() {
        if i == 0 {
            match decl {
                Decl::Mod(_) => {}
                _ => error_store.push("first decl must be a module name!".to_string()),
            }
        }

        bind_decl(sym_table, &mut error_store, decl)
    }

    error_store
}

fn bind_decl(sym_table: &mut SymTable, error_store: &mut ErrorStore, decl: &Decl) {
    match decl {
        Decl::Function(decl) => {
            todo!()
        }
        Decl::Type(decl) => {
            todo!()
        }
        Decl::Const(decl) => {
            sym_table.add_binding(Binding {
                name: decl.name.clone(),
                // TODO: we don't want to bind type_spec values yet
                type_spec: TypeSpec::InvalidType,
            });

            bind_expr(sym_table, error_store, &decl.value);
        }
        Decl::Var(decl) => {
            sym_table.add_binding(Binding {
                name: decl.name.clone(),
                // TODO: we don't want to bind type_spec values yet
                type_spec: TypeSpec::InvalidType,
            });

            bind_expr(sym_table, error_store, &decl.value)
        }
        Decl::Use(decl) => match sym_table.add_use_modules(decl.modules.clone()) {
            Ok(_) => {}
            // TODO: make it easier to just add an error instead of a string?
            Err(e) => error_store.push(e.msg),
        },
        Decl::Mod(decl) => match sym_table.set_module(decl.name.clone()) {
            Ok(_) => {}
            // TODO: make it easier to just add an error instead of a string?
            Err(e) => error_store.push(e.msg),
        },
    }
}

fn bind_stmt(sym_table: &mut SymTable, error_store: &mut ErrorStore, stmt: Stmt) {
    todo!()
}

fn bind_expr(sym_table: &mut SymTable, error_store: &mut ErrorStore, expr: &Expr) {
    match expr {
        Expr::IntLiteral(_) => { /* nothing to bind */ }
        Expr::FloatLiteral(_) => { /* nothing to bind */ }
        Expr::StringLiteral(_) => { /* nothing to bind */ }
        Expr::BoolLiteral(_) => { /* nothing to bind */ }
        Expr::Identifier(expr) => match sym_table.get_binding(&expr.name) {
            Some(_) => {}
            None => error_store.push(format!("unknown identifier {:?}", &expr.name)),
        },
        Expr::Binary(expr) => {
            bind_expr(sym_table, error_store, &expr.left);
            bind_expr(sym_table, error_store, &expr.right);
        }
        Expr::Unary(expr) => {
            bind_expr(sym_table, error_store, &expr.operand);
        }
        Expr::Call(expr) => {
            todo!()
        }
        Expr::Assignment(expr) => {
            todo!()
        }
        Expr::Index(expr) => {
            todo!()
        }
        Expr::Range(expr) => {
            todo!()
        }
        Expr::DotAccess(expr) => {
            todo!()
        }
        Expr::ModuleAccess(expr) => {
            todo!()
        }
        Expr::MetaType(expr) => {
            todo!()
        }
        Expr::Alloc(expr) => {
            todo!()
        }
        Expr::Free(expr) => {
            todo!()
        }
    };
}
