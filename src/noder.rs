use crate::ast::{Decl, Expr, Module, TypeSpec};
use crate::file_set::{FileSet, StrID};
use crate::hir::{Node, NodeID, NodeTree};

struct Binding {
    name: StrID,
    // This jus indicates that it was EVER initialized in any possible
    // flow, not that it's initialized in EVRY possible flow.
    // that analisys will take place later, but we can use this to
    // identify early errors
    initialized: bool,
    used: bool,
    mutable: bool,
    node: NodeID,
}

struct Type {
    name: StrID,
    kind: TypeKind,
    node: NodeID,
}

#[derive(Copy, Clone)]
enum TypeKind {
    Unit,
    Struct,
    Enum,
    Callable,
    Indexable,
}

type ScopeID = usize;

struct Scope {
    parent: Option<ScopeID>,
    bindings: Vec<Binding>,
    types: Vec<Type>,
}

impl Scope {
    fn new(parent: ScopeID) -> Self {
        Scope {
            parent: Some(parent),
            bindings: vec![],
            types: vec![],
        }
    }

    fn new_root() -> Self {
        Scope {
            parent: None,
            bindings: vec![],
            types: vec![],
        }
    }
}

struct SymTable<'a> {
    fset: &'a FileSet<'a>,
    scopes: Vec<Scope>,
    current_scope: ScopeID,
}

impl<'a> SymTable<'a> {
    fn new(fset: &'a FileSet<'a>) -> Self {
        SymTable {
            fset,
            scopes: vec![Scope::new_root()],
            current_scope: 0,
        }
    }

    fn open_scope(&mut self) {
        let parent = self.current_scope;
        self.scopes.push(Scope::new(parent));

        self.current_scope = self.scopes.len() - 1;
    }

    fn close_scope(&mut self) {
        let scope = self.must_get_scope();
        match scope.parent {
            Some(s) => self.current_scope = s,
            None => panic!("can not close the root scope"),
        }
    }

    fn add_binding(&mut self, b: Binding) {
        let scope = self.must_get_mut_scope();
        scope.bindings.push(b);
    }

    fn add_type(&mut self, t: Type) {
        let scope = self.must_get_mut_scope();
        scope.types.push(t)
    }

    fn get_binding(&self, name: StrID) -> Option<&Binding> {
        let mut scope = Some(self.must_get_scope());
        while scope.is_some() {
            let s = scope.unwrap();
            match s.bindings.iter().find(|b| b.name == name) {
                Some(b) => return Some(b),
                None => match s.parent {
                    Some(s) => scope = self.scopes.get(s),
                    None => scope = None,
                },
            }
        }

        None
    }

    fn get_type(&self, name: StrID) -> Option<&Type> {
        let mut scope = Some(self.must_get_scope());
        while scope.is_some() {
            let s = scope.unwrap();
            match s.types.iter().find(|t| t.name == name) {
                Some(t) => return Some(t),
                None => match s.parent {
                    Some(s) => scope = self.scopes.get(s),
                    None => scope = None,
                },
            }
        }

        None
    }

    fn must_get_scope(&self) -> &Scope {
        match self.scopes.get(self.current_scope) {
            Some(s) => s,
            None => panic!("invalid ScopeID in SymTable"),
        }
    }

    fn must_get_mut_scope(&mut self) -> &mut Scope {
        match self.scopes.get_mut(self.current_scope) {
            Some(s) => s,
            None => panic!("invalid ScopeID in SymTable"),
        }
    }
}

pub struct Noder {}

impl Noder {
    pub fn new() -> Self {
        Noder {}
    }

    pub fn node(&self, fset: &FileSet, module: Module) -> NodeTree {
        // Should these types be owned by the Noder type?
        let mut node_tree = NodeTree::new();
        let mut sym_table = SymTable::new(fset);

        for (i, decl) in module.into_iter().enumerate() {
            if i == 0 && !matches!(decl, Decl::Mod(_)) {
                // TODO: should be a error not a panic like this
                // update it once we have a good error system
                panic!("first line must be a module")
            }

            self.node_decl(&mut sym_table, &mut node_tree, decl);
        }
        todo!()
    }

    fn node_decl(&self, sym_table: &mut SymTable, node_tree: &mut NodeTree, decl: &Decl) {
        match decl {
            Decl::Function(_) => todo!(),
            Decl::Type(decl) => {
                // create the node
                let decl_id = node_tree.add_root_node(Node::TypeDecl {
                    name: decl.name,
                    type_spec: decl.type_spec.clone(),
                });

                let kind = match decl.type_spec {
                    TypeSpec::Array(_) => TypeKind::Indexable,
                    TypeSpec::Slice(_) => TypeKind::Indexable,
                    TypeSpec::Enum(_) => TypeKind::Enum,
                    TypeSpec::Struct(_) => TypeKind::Struct,
                    TypeSpec::Named { module, name } => {
                        if module.is_some() {
                            todo!("need to handle modules");
                        }

                        match sym_table.get_type(name) {
                            Some(t) => t.kind,
                            // TODO: use the error reporter instead of just panicing
                            None => panic!("unknown type!"),
                        }
                    }
                    _ => TypeKind::Unit, // all other types are unit types
                };

                sym_table.add_type(Type {
                    name: decl.name,
                    kind,
                    node: decl_id,
                });
            }
            Decl::Const(decl) => {
                // create the nodes
                let decl_id = node_tree.add_root_node(Node::VarDecl { name: decl.name });
                let value_node = self.node_expr(&decl.value);
                node_tree.add_node(Node::Assign {
                    target: decl_id,
                    value: value_node,
                });

                // update the symbol table
                sym_table.add_binding(Binding {
                    name: decl.name,
                    initialized: true,
                    used: false,
                    mutable: false,
                    node: decl_id,
                });
            }
            Decl::Var(decl) => {
                // create the nodes
                let decl_id = node_tree.add_root_node(Node::VarDecl { name: decl.name });
                let value_node = self.node_expr(&decl.value);
                node_tree.add_node(Node::Assign {
                    target: decl_id,
                    value: value_node,
                });

                // update the symbol table
                sym_table.add_binding(Binding {
                    name: decl.name,
                    initialized: true,
                    used: false,
                    mutable: true,
                    node: decl_id,
                });
            }
            Decl::Use(_) => todo!(
                "need to resolve the import graph and then import modules, this is unsupported for now"
            ),
            Decl::Mod(decl) => {
                let result = node_tree.set_module_name(decl.name);
                if result.is_err() {
                    panic!("error setting node name")
                }
            }
        }
    }
    fn walk_block(&self) {}
    fn node_stmt(&self) {}
    fn node_expr(&self, expr: &Expr) -> NodeID {
        todo!()
    }
}
