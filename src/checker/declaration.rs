
pub struct DeclChecker {

}

impl DeclChecker{
    fn check_decl(&mut self, decl: Decl) {
        match decl {
            Decl::Const(decl) => {
                let type_spec = self.check_expr(decl.value);
                self.sym_table.add_binding(Binding {
                    name: decl.name,
                    type_spec,
                })
            }
            Decl::Var(decl) => {
                let type_spec = self.check_expr(decl.value);
                self.sym_table.add_binding(Binding {
                    name: decl.name,
                    type_spec,
                })
            }
            Decl::Type(decl) => self.sym_table.add_type(Type {
                name: decl.name.name,
                type_spec: decl.type_spec,
            }),
            Decl::Mod(_) => self.add_err("only one module declaration is allowed"),
            // TODO: we should parse the use block first to resovle the DAG for module parsing
            // Then we can parse from leaves to roots so all types can be fully resolved
            Decl::Use(_) => { /* nee to import modules here leave as a no-op for now*/ }
            Decl::Function(decl) => {
                self.sym_table.push_scope();

                for param in decl.params {
                    self.sym_table.add_binding(Binding {
                        name: param.name,
                        type_spec: param.type_spec,
                    });
                }

                self.sym_table.set_return_type(decl.return_type);

                self.check_stmts(decl.body.statements);

                self.sym_table.pop_scope();
            }
        }
    }
}
