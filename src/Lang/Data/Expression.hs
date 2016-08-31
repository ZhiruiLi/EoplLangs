module Lang.Data.Expression where

data Expression =
    ConstExpr Integer
  | DiffExpr Expression Expression
  | IsZeroExpr Expression
  | IfExpr Expression Expression Expression
  | VarExpr String
  | LetExpr String Expression Expression
  | MinusExpr Expression

  deriving(Show, Eq)
