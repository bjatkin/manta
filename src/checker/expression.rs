use crate::ast::{Expr, TypeSpec};
use crate::checker::ErrorStore;
use crate::checker::sym_table::SymTable;

pub struct ExprChecker;

impl ExprChecker {
    pub fn check(
        &mut self,
        error_store: &mut ErrorStore,
        sym_table: &mut SymTable,
        expr: Expr,
    ) -> TypeSpec {
        match expr {
            Expr::IntLiteral(_) => TypeSpec::UnsizedInt,
            Expr::FloatLiteral(_) => TypeSpec::UnsizedFloat,
            Expr::StringLiteral(_) => TypeSpec::String,
            Expr::BoolLiteral(_) => TypeSpec::Bool,
            Expr::Identifier(expr) => {
                let binding = sym_table.get_binding(&expr.name);
                match binding {
                    Some(b) => b.type_spec.clone(),
                    None => panic!("binding does not exist"),
                }
            }
            Expr::Binary(expr) => {
                let left = self.check(error_store, sym_table, *expr.left);
                let right = self.check(error_store, sym_table, *expr.right);

                // TODO: not all operators support all operand types
                if left != right {
                    error_store.push(format!("{:?} does not match {:?}", left, right))
                }

                left
            }
            Expr::Unary(expr) => self.check(error_store, sym_table, *expr.operand),
            Expr::Call(expr) => todo!("need to implement"),
            Expr::Assignment(expr) => todo!("should this actually be an expression"),
            Expr::Index(expr) => {
                let target_type = self.check(error_store, sym_table, *expr.target);
                match target_type {
                    TypeSpec::Slice(t) => *t,
                    TypeSpec::Array(t) => *t.type_spec,
                    _ => {
                        error_store.push("can not index this expression".to_string());
                        todo!("what type should I return here")
                    }
                }
            }
            Expr::Range(expr) => todo!("return range type"),
            Expr::DotAccess(expr) => todo!("in progress"),
            Expr::ModuleAccess(expr) => todo!("need to pull in modules for this to work"),
            Expr::MetaType(expr) => expr.type_spec,
            Expr::Alloc(expr) => todo!("need to return unsafe::ptr here right?"),
            Expr::Free(expr) => TypeSpec::None, // can a global free like this fail?
        }
    }
}
