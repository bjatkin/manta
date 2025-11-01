enum Operator {
    Add,
    Subtract,
    Multiply,
    Divide,
} 

struct BinaryOp {
    left: Box<Expr>,
    operator: Operator,
    right: Box<Expr>,
}

enum Expr {
    BinaryOp(BinaryOp),
}