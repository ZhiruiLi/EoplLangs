module LetLang.Data.Expression where


data Expression =
    ConstExpr Integer
  | IsZeroExpr Expression
  | IfExpr Expression Expression Expression
  | VarExpr String
  | LetExpr String Expression Expression
  | MinusExpr Expression
  | BinOpExpr Op Expression Expression
  deriving(Show, Eq)

data Op =
  Add | Sub | Mul | Div | Gt | Le | Eq
  deriving(Show, Eq)

