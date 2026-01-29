use crate::ast::TypeSpec;
use crate::checker::CheckerError;

pub struct Type {
    pub name: String,
    pub type_spec: TypeSpec,
}

pub struct Binding {
    pub name: String,
    pub type_spec: TypeSpec,
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
    module: Option<String>,
    use_modules: Option<Vec<String>>,
    scopes: Vec<Scope>,
    current_scope: ScopeID,
}

impl SymTable {
    pub fn new() -> Self {
        SymTable {
            module: None,
            use_modules: None,
            scopes: vec![],
            current_scope: 0,
        }
    }

    pub fn set_module(&mut self, module: String) -> Result<(), CheckerError> {
        match &self.module {
            Some(module) => Err(CheckerError::new(format!(
                "module is already named {:?}",
                module
            ))),
            None => {
                self.module = Some(module);
                Ok(())
            }
        }
    }

    pub fn add_use_modules(&mut self, modules: Vec<String>) -> Result<(), CheckerError> {
        match &self.use_modules {
            Some(_) => Err(CheckerError::new(
                "only 1 use block allowed per file".to_string(),
            )),
            None => {
                self.use_modules = Some(modules);
                Ok(())
            }
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
