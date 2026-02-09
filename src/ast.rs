use serde::{Deserialize, Serialize};
use std::marker::PhantomData;

use crate::str_store::StrID;

pub struct StoreID<T> {
    pub id: usize,
    // This costs zero bytes at runtime
    _marker: PhantomData<T>,
}

pub struct Store<T> {
    items: Vec<T>,
}

impl<T> Store<T> {
    pub fn new() -> Self {
        Store { items: vec![] }
    }

    pub fn add(&mut self, item: T) -> StoreID<T> {
        self.items.push(item);

        StoreID {
            id: self.items.len() - 1,
            _marker: PhantomData,
        }
    }

    pub fn get(&self, id: StoreID<T>) -> Option<&T> {
        self.items.get(id.id)
    }
}

pub type DeclID = usize;
pub type StmtID = usize;
pub type ExprID = usize;
pub type TypeSpecID = usize;
pub type PatternID = usize;

pub struct NodeStore {
    decls: Vec<Decl>,
    stmts: Vec<Stmt>,
    exprs: Vec<Expr>,
    type_specs: Vec<TypeSpec>,
    patterns: Vec<Pattern>,
}

impl NodeStore {
    pub fn new() -> Self {
        NodeStore {
            decls: vec![],
            stmts: vec![],
            exprs: vec![],
            type_specs: vec![],
            patterns: vec![],
        }
    }

    pub fn add_decl(&mut self, decl: Decl) -> DeclID {
        self.decls.push(decl);
        self.decls.len() - 1
    }

    pub fn get_decl(&self, id: DeclID) -> Option<&Decl> {
        self.decls.get(id)
    }

    pub fn add_stmt(&mut self, stmt: Stmt) -> StmtID {
        self.stmts.push(stmt);
        self.stmts.len() - 1
    }

    pub fn get_stmt(&self, id: StmtID) -> Option<&Stmt> {
        self.stmts.get(id)
    }

    pub fn add_expr(&mut self, expr: Expr) -> ExprID {
        self.exprs.push(expr);
        self.exprs.len() - 1
    }

    pub fn get_expr(&self, id: ExprID) -> Option<&Expr> {
        self.exprs.get(id)
    }

    pub fn add_type_spec(&mut self, type_spec: TypeSpec) -> TypeSpecID {
        self.type_specs.push(type_spec);
        self.type_specs.len() - 1
    }

    pub fn get_type_spec(&self, id: TypeSpecID) -> Option<&TypeSpec> {
        self.type_specs.get(id)
    }

    pub fn add_pattern(&mut self, pattern: Pattern) -> PatternID {
        self.patterns.push(pattern);
        self.patterns.len() - 1
    }

    pub fn get_pattern(&self, id: PatternID) -> Option<&Pattern> {
        self.patterns.get(id)
    }
}

impl<'a> IntoIterator for &'a NodeStore {
    type Item = &'a Decl;
    type IntoIter = NodeStoreIter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        NodeStoreIter {
            node_store: self,
            idx: 0,
        }
    }
}

pub struct NodeStoreIter<'a> {
    node_store: &'a NodeStore,
    idx: usize,
}

impl<'a> Iterator for NodeStoreIter<'a> {
    type Item = &'a Decl;

    fn next(&mut self) -> Option<Self::Item> {
        match self.node_store.decls.get(self.idx) {
            Some(decl) => {
                self.idx += 1;
                Some(decl)
            }
            None => None,
        }
    }
}

/// Top-level declarations in a Manta program
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum Decl {
    Function(FunctionDecl),
    Type(TypeDecl),
    Const(ConstDecl),
    Var(VarDecl),
    Use(UseDecl),
    Mod(ModDecl),
    Invalid,
}

/// Function declaration
///
/// Example:
/// ```manta
/// fn add(a, b i32) i32 {
///     return a + b
/// }
/// ```
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct FunctionDecl {
    pub name: StrID,
    pub params: Vec<Parameter>,
    pub return_type: Option<TypeSpec>,
    pub body: BlockStmt,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct TypeDecl {
    pub name: StrID,
    pub type_spec: TypeSpec,
}

/// Function parameter
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct Parameter {
    pub name: StrID,
    pub type_spec: TypeSpec,
}

/// Const declaration
///
/// Example:
/// ```manta
/// const PI = 3.14159
/// ```
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct ConstDecl {
    pub name: StrID,
    pub value: Expr,
}

/// Var declaration
///
/// Example:
/// ```manta
/// var status = "Ok"
/// ```
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct VarDecl {
    pub name: StrID,
    pub value: Expr,
}

/// Use declaration
///
/// Example:
/// ```manta
/// import "math"
/// import ("std", "io")
/// ```
#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct UseDecl {
    pub modules: Vec<StrID>,
}

/// Mod declaration
///
/// Example:
/// ```manta
/// mod main
/// ```
#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct ModDecl {
    pub name: StrID,
}

/// Type specification
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum TypeSpec {
    Int32,
    Int16,
    Int8,
    Int64,
    UInt32,
    UInt16,
    UInt8,
    UInt64,
    Float32,
    Float64,
    String,
    Bool,
    // User-defined types
    Named { module: Option<StrID>, name: StrID },
    // Composite types
    Pointer(TypeSpecID),
    Slice(TypeSpecID),
    Array(ArrayType),
    Struct(StructType),
    Enum(EnumType),
}

/// MetaType
#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct MetaTypeExpr {
    pub type_spec: TypeSpec,
}

/// Array type with size
#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct ArrayType {
    pub type_spec: TypeSpecID,
    pub size: usize,
}

/// Struct type with named fields
#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct StructType {
    pub fields: Vec<StructField>,
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct StructField {
    pub name: StrID,
    pub type_spec: TypeSpec,
}

/// Enum type with named variants
#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct EnumType {
    pub variants: Vec<EnumVariant>,
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct EnumVariant {
    pub name: StrID,
    pub payload: Option<TypeSpec>,
}

/// A block of statements
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct BlockStmt {
    pub statements: Vec<Stmt>,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct IfStmt {
    pub check: ExprID,
    pub success: BlockStmt,
    pub fail: Option<BlockStmt>,
}

/// Statements in a block
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum Stmt {
    Let(LetStmt),
    Assign(AssignStmt),
    Expr(ExprStmt),
    Return(ReturnStmt),
    Defer(DeferStmt),
    Match(MatchStmt),
    Block(BlockStmt),
    If(IfStmt),
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct LetStmt {
    pub pattern: Pattern,
    pub value: Expr,
    pub except: LetExcept,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum LetExcept {
    Or {
        binding: Option<StrID>,
        body: BlockStmt,
    },
    Wrap(Expr),
    Panic,
    None,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct AssignStmt {
    pub lvalue: Expr,
    pub rvalue: Expr,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct ExprStmt {
    pub expr: Expr,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct ReturnStmt {
    pub value: Option<Expr>,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct DeferStmt {
    pub block: BlockStmt,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct MatchStmt {
    pub target: Expr,
    pub arms: Vec<MatchArm>,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub body: BlockStmt,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Pattern {
    IntLiteral(i64),
    StringLiteral(StrID),
    BoolLiteral(bool),
    FloatLiteral(f64),

    TypeSpec(TypeSpec),

    Payload(PayloadPat),
    ModuleAccess(ModuleAccesPat),
    DotAccess(DotAccessPat),

    // TODO: should this just wrap a StrID?
    Identifier(IdentifierPat),
    Default, // the _ pattern
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ModuleAccesPat {
    pub module: PatternID,
    pub pat: PatternID,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct DotAccessPat {
    pub target: Option<PatternID>,
    pub field: IdentifierPat,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct PayloadPat {
    pub pat: PatternID,
    pub payload: StrID,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct EnumVariantPat {
    pub type_name: Option<StrID>,
    pub name: StrID,
    pub payload_binding: Option<StrID>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct IdentifierPat {
    pub name: StrID,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum Expr {
    // Literals
    IntLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(StrID),
    BoolLiteral(bool),

    // Identifiers and references
    Identifier(IdentifierExpr),

    // Operations
    Binary(BinaryExpr),
    Unary(UnaryExpr),

    // Function call
    Call(CallExpr),

    // Indexing for an expression
    Index(IndexExpr),

    // Range expressions like 1:3
    Range(RangeExpr),

    // Accessing a field for a struct or enum
    DotAccess(DotAccessExpr),

    // Accessing a member of a module
    ModuleAccess(ModuleAccessExpr),

    // Mete Type expression
    MetaType(MetaTypeExpr),

    // Memory operations
    Alloc(AllocExpr),
    Free(FreeExpr),
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct IdentifierExpr {
    pub name: StrID,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct BinaryExpr {
    pub left: ExprID,
    pub operator: BinaryOp,
    pub right: ExprID,
}

#[derive(Debug, PartialEq, Clone, Copy, Serialize, Deserialize)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    LogicalAnd,
    LogicalOr,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct UnaryExpr {
    pub operator: UnaryOp,
    pub operand: ExprID,
}

#[derive(PartialEq, Debug, Clone, Copy, Serialize, Deserialize)]
pub enum UnaryOp {
    Not,
    Negate,
    Positive,
    Dereference,
    AddressOf,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct CallExpr {
    pub func: ExprID,
    pub args: Vec<Expr>,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct IndexExpr {
    pub target: ExprID,
    pub index: ExprID,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct RangeExpr {
    pub start: ExprID,
    pub end: ExprID,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct DotAccessExpr {
    // this is an option because this can be infered in some contexts
    pub target: Option<ExprID>,
    pub field: StrID,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct ModuleAccessExpr {
    pub module: StrID,
    pub expr: ExprID,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct CastExpr {
    expr: ExprID,
    target_type: TypeSpec,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct AllocExpr {
    pub meta_type: ExprID,
    pub options: Vec<Expr>,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct FreeExpr {
    pub expr: ExprID,
}
