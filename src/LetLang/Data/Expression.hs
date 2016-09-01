module LetLang.Data.Expression where


data Expression =
    ConstExpr Integer
  | IfExpr Expression Expression Expression
  | VarExpr String
  | LetExpr String Expression Expression
  | BinOpExpr BinOp Expression Expression
  | UnaryOpExpr UnaryOp Expression
  | EmptyListExpr
  deriving(Show, Eq)

data BinOp =
  Add | Sub | Mul | Div | Gt | Le | Eq | Cons
  deriving(Show, Eq)

data UnaryOp = Car | Cdr | Minus | IsZero
  deriving(Show, Eq)
