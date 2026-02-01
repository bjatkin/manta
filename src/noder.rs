use crate::ast::{Decl, Expr, Module, TypeSpec};
use crate::hir::{Node, NodeID, NodeTree};
use crate::parser::lexer::keywords;
use crate::str_store::{StrID, StrStore};

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

    fn new_root(str_store: &StrStore) -> Self {
        Scope {
            parent: None,
            bindings: vec![],
            types: vec![
                // make sure all the builtin types are always available in the root scope
                Type {
                    name: str_store.get_keyword(keywords::U8),
                    kind: TypeKind::Unit,
                    node: 0,
                },
                Type {
                    name: str_store.get_keyword(keywords::U16),
                    kind: TypeKind::Unit,
                    node: 0,
                },
                Type {
                    name: str_store.get_keyword(keywords::U32),
                    kind: TypeKind::Unit,
                    node: 0,
                },
                Type {
                    name: str_store.get_keyword(keywords::U64),
                    kind: TypeKind::Unit,
                    node: 0,
                },
                Type {
                    name: str_store.get_keyword(keywords::I8),
                    kind: TypeKind::Unit,
                    node: 0,
                },
                Type {
                    name: str_store.get_keyword(keywords::I16),
                    kind: TypeKind::Unit,
                    node: 0,
                },
                Type {
                    name: str_store.get_keyword(keywords::I32),
                    kind: TypeKind::Unit,
                    node: 0,
                },
                Type {
                    name: str_store.get_keyword(keywords::I64),
                    kind: TypeKind::Unit,
                    node: 0,
                },
                Type {
                    name: str_store.get_keyword(keywords::F32),
                    kind: TypeKind::Unit,
                    node: 0,
                },
                Type {
                    name: str_store.get_keyword(keywords::F64),
                    kind: TypeKind::Unit,
                    node: 0,
                },
                Type {
                    name: str_store.get_keyword(keywords::BOOL),
                    kind: TypeKind::Unit,
                    node: 0,
                },
                Type {
                    name: str_store.get_keyword(keywords::STR),
                    kind: TypeKind::Struct,
                    node: 0,
                },
            ],
        }
    }
}

struct SymTable {
    scopes: Vec<Scope>,
    current_scope: ScopeID,
}

impl SymTable {
    fn new(str_store: &StrStore) -> Self {
        let root = Scope::new_root(str_store);
        SymTable {
            scopes: vec![root],
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

    pub fn node(&self, str_store: &StrStore, module: Module) -> NodeTree {
        // Should these types be owned by the Noder type?
        let mut node_tree = NodeTree::new();
        let mut sym_table = SymTable::new(str_store);

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
    fn node_expr(&self, _expr: &Expr) -> NodeID {
        todo!()
    }
}
