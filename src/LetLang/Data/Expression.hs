module LetLang.Data.Expression where

import qualified LetLang.Data.ExpressedValue as V

data Expression =
    ConstExpr V.ExpressedValue
  | VarExpr String
  | LetExpr [(String, Expression)] Expression
  | LetStarExpr [(String, Expression)] Expression
  | BinOpExpr BinOp Expression Expression
  | UnaryOpExpr UnaryOp Expression
  | EmptyListExpr
  | ListExpr [Expression]
  | CondExpr [(Expression, Expression)]
  deriving(Show, Eq)

data BinOp =
  Add | Sub | Mul | Div | Gt | Le | Eq | Cons
  deriving(Show, Eq)

data UnaryOp = Car | Cdr | Minus | IsZero
  deriving(Show, Eq)
