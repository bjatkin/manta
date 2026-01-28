use crate::ast::{Decl, Expr, Stmt, TypeSpec};

pub struct Type {
    name: String,
    type_spec: TypeSpec,
}

pub struct Binding {
    name: String,
    type_spec: TypeSpec,
}

type ScopeID = usize;

pub struct Scope {
    parent: Option<ScopeID>,
    types: Vec<Type>,
    bindings: Vec<Binding>,
    return_type: Option<TypeSpec>,
}

impl Scope {
    pub fn new(parent: ScopeID) -> Self {
        Scope {
            parent: Some(parent),
            types: vec![],
            bindings: vec![],
            return_type: None,
        }
    }
}

pub struct SymTable {
    scopes: Vec<Scope>,
    current_scope: ScopeID,
}

impl SymTable {
    pub fn new() -> Self {
        SymTable {
            scopes: vec![],
            current_scope: 0,
        }
    }

    pub fn add_type(&mut self, t: Type) {
        let scope = self.scopes.get_mut(self.current_scope);
        match scope {
            Some(s) => s.types.push(t),
            None => panic!("null scope dereference"),
        }
    }

    pub fn get_type(&mut self, name: &str) -> Option<&Type> {
        let mut scope_id = Some(self.current_scope);
        loop {
            if let Some(id) = scope_id {
                let scope = self.scopes.get(id);
                match scope {
                    Some(s) => {
                        for t in &s.types {
                            if t.name == name {
                                return Some(t);
                            }
                        }
                        scope_id = s.parent;
                    }
                    None => panic!("null scope dereference"),
                }
            } else {
                return None;
            }
        }
    }

    pub fn add_binding(&mut self, b: Binding) {
        let scope = self.scopes.get_mut(self.current_scope);
        match scope {
            Some(s) => s.bindings.push(b),
            None => panic!("null scope dereference"),
        }
    }

    pub fn get_binding(&mut self, name: &str) -> Option<&Binding> {
        let mut scope_id = Some(self.current_scope);
        loop {
            if let Some(id) = scope_id {
                let scope = self.scopes.get(id);
                match scope {
                    Some(s) => {
                        for b in &s.bindings {
                            if b.name == name {
                                return Some(b);
                            }
                        }
                        scope_id = s.parent;
                    }
                    None => panic!("null scope dereference"),
                }
            } else {
                return None;
            }
        }
    }

    pub fn set_return_type(&mut self, return_type: Option<TypeSpec>) {
        let scope = self.scopes.get_mut(self.current_scope);
        match scope {
            Some(s) => s.return_type = return_type,
            None => panic!("null scope dereference"),
        }
    }

    pub fn get_return_type(&self) -> Option<TypeSpec> {
        let scope = self.scopes.get(self.current_scope);
        match scope {
            Some(s) => s.return_type.clone(),
            None => panic!("null scope dereference"),
        }
    }

    pub fn push_scope(&mut self) {
        let new_scope = Scope::new(self.current_scope);
        self.current_scope += 1;

        if self.current_scope >= self.scopes.len() {
            self.scopes.push(new_scope);
        } else {
            self.scopes[self.current_scope] = new_scope;
        }
    }

    pub fn pop_scope(&mut self) {
        self.current_scope -= 1
    }
}

pub struct Checker {
    module: String,
    sym_table: SymTable,
}

impl Checker {
    pub fn new() -> Self {
        Checker {
            module: "".to_string(),
            sym_table: SymTable::new(),
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
                    panic!("first decl must be the module name");
                }
            } else {
                self.check_decl(decl);
            }

            first = false;
        }
    }

    fn check_decl(&mut self, decl: Decl) {
        match decl {
            Decl::Const(decl) => self.sym_table.add_binding(Binding {
                name: decl.name,
                // TODO: need to infer the type based on the RHS expression
                type_spec: TypeSpec::Int64,
            }),
            Decl::Var(decl) => self.sym_table.add_binding(Binding {
                name: decl.name,
                // TODO: need to infer the type based on the RHS expression
                type_spec: TypeSpec::Int64,
            }),
            Decl::Type(decl) => self.sym_table.add_type(Type {
                name: decl.name.name,
                type_spec: decl.type_spec,
            }),
            Decl::Mod(_) => panic!("only one module declaration per file"),
            // TODO: we should parse the use block first to resovle the DAG for module parsing
            // Then we can parse from leaves to roots so all types can be fully resolved
            Decl::Use(_) => { /* nee to implrt modules here leave as a no-op for now*/ }
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

    fn check_stmts(&mut self, stmts: Vec<Stmt>) {
        for stmt in stmts {
            match stmt {
                Stmt::Let(stmt) => {
                    todo!("this is actually pretty trick because let is pretty powerful")
                }
                Stmt::Assign(stmt) => {
                    // TODO: need to check the type_spec of stmt.rvalue
                    match stmt.lvalue {
                        Expr::Identifier(expr) => {
                            let name = self.sym_table.get_binding(&expr.name);
                            if name.is_none() {
                                panic!("can not assign to Identifier that is not declared");
                            }
                        }
                        _ => todo!("support assigning to other expression types"),
                    }
                }
                Stmt::Expr(stmt) => {
                    self.check_expr(stmt.expr);
                }
                Stmt::Return(stmt) => {
                    // TODO: get the type of the expression (stmt.value)

                    match self.sym_table.get_return_type() {
                        Some(t) => {
                            if stmt.value.is_none() {
                                panic!("missing return!")
                            }

                            let type_spec = self.check_expr(stmt.value.unwrap());

                            // TODO: something better than panicing would be good
                            assert_eq!(t, type_spec);
                        }
                        None => {
                            if stmt.value.is_some() {
                                panic!("no return value expected")
                            }
                        }
                    }
                }
                Stmt::Defer(stmt) => self.check_stmts(stmt.block.statements),
                Stmt::Match(stmt) => todo!("need to support this"),
                Stmt::Block(stmt) => self.check_stmts(stmt.statements),
                Stmt::If(stmt) => todo!("need to support this"),
            }
        }
    }

    fn check_expr(&mut self, expr: Expr) -> TypeSpec {
        match expr {
            Expr::IntLiteral(_) => TypeSpec::UnsizedInt,
            Expr::FloatLiteral(_) => TypeSpec::UnsizedFloat,
            Expr::StringLiteral(_) => TypeSpec::String,
            Expr::BoolLiteral(_) => TypeSpec::Bool,
            Expr::Identifier(expr) => {
                let binding = self.sym_table.get_binding(&expr.name);
                match binding {
                    Some(b) => b.type_spec.clone(),
                    None => panic!("binding does not exist"),
                }
            }
            Expr::Binary(expr) => {
                let left = self.check_expr(*expr.left);
                let right = self.check_expr(*expr.right);

                // TODO: should be a better error here
                // Also, not all operators support all operand types
                assert_eq!(left, right);

                left
            }
            Expr::Unary(expr) => self.check_expr(*expr.operand),
            Expr::Call(expr) => todo!("need to implement"),
            _ => todo!("in progress"),
        }
    }
}
