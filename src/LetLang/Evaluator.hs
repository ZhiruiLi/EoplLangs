module LetLang.Evaluator
( valueOf
, run
, eval
, evalProgram
) where

import           LetLang.Data.Environment
import           LetLang.Data.ExpressedValue
import           LetLang.Data.Expression
import           LetLang.Data.Program
import           LetLang.Parser

run :: String -> Either String ExpressedValue
run input = parseProgram input >>= evalProgram

eval :: Expression -> Either String ExpressedValue
eval = flip valueOf empty

evalProgram :: Program -> Either String ExpressedValue
evalProgram (Program expr) = eval expr

valueOf :: Expression -> Environment -> Either String ExpressedValue
valueOf (ConstExpr n) _   = Right $ NumVal n
valueOf (VarExpr var) env = case applySafe env var of
  Nothing  -> Left $ "Not in scope: " `mappend` var
  Just val -> Right val
-- begin operate on list
valueOf EmptyListExpr _ = Right $ ListVal Empty
valueOf (BinOpExpr Cons expr1 expr2) env =
  case (valueOf expr1 env, valueOf expr2 env) of
    (msg@(Left _), _) -> msg
    (_, msg@(Left _)) -> msg
    (Right v, Right (ListVal lst)) -> Right $ ListVal (NonEmpty v lst)
    (_, Right v) -> Left $
      "Tail of a list should be list, but got: " `mappend` show v
valueOf (UnaryOpExpr Car expr) env =
  case valueOf expr env of
    msg@(Left _) -> msg
    Right (ListVal Empty) -> Left "Could not apply 'car' on empty list"
    Right (ListVal (NonEmpty v _)) -> Right v
    Right v -> Left $
      "Operand of 'car' should be list, but got: " `mappend` show v
valueOf (UnaryOpExpr Cdr expr) env =
  case valueOf expr env of
    msg@(Left _) -> msg
    Right (ListVal Empty) -> Left "Could not apply 'cdr' on empty list"
    Right (ListVal (NonEmpty _ v)) -> Right $ ListVal v
    Right v -> Left $
      "Operand of 'cdr' should be list, but got: " `mappend` show v
-- end operate on list
valueOf (BinOpExpr op expr1 expr2) env =
  evalBinOpExpr op expr1 expr2 env
valueOf (UnaryOpExpr op expr) env =
  evalUnaryOpExpr op expr env
valueOf (IfExpr expr1 expr2 expr3) env =
  case valueOf expr1 env of
    Right (BoolVal b) -> valueOf (if b then expr2 else expr3) env
    Right x -> Left $
        "Predicate of if-expression should be boolean, but got: " `mappend`
        show x
    left -> left
valueOf (LetExpr var valExpr expr) env =
  case valueOf valExpr env of
    Left msg  -> Left msg
    Right val -> valueOf expr (extend var val env)

binBoolOpMap :: [(BinOp, Bool -> Bool -> Bool)]
binBoolOpMap = []

binNumToNumOpMap :: [(BinOp, Integer -> Integer -> Integer)]
binNumToNumOpMap = [(Add, (+)), (Sub, (-)), (Mul, (*)), (Div, div)]

binNumToBoolOpMap :: [(BinOp, Integer -> Integer -> Bool)]
binNumToBoolOpMap = [(Gt, (>)), (Le, (<)), (Eq, (==))]

unaryBoolOpMap :: [(UnaryOp, Bool -> Bool)]
unaryBoolOpMap = []

unaryNumToNumOpMap :: [(UnaryOp, Integer -> Integer)]
unaryNumToNumOpMap = [(Minus, negate)]

unaryNumToBoolOpMap :: [(UnaryOp, Integer -> Bool)]
unaryNumToBoolOpMap = [(IsZero, (0 ==))]


evalBinOpExpr :: BinOp
              -> Expression
              -> Expression
              -> Environment
              -> Either String ExpressedValue
evalBinOpExpr op expr1 expr2 env =
  case (wrapVal1, wrapVal2) of
    (msg@(Left _), _) -> msg
    (_, msg@(Left _)) -> msg
    (Right val1, Right val2) ->
      case ( lookup op binNumToNumOpMap
           , lookup op binNumToBoolOpMap
           , lookup op binBoolOpMap
           ) of
        (Just func, _, _) -> case (val1, val2) of
          (NumVal n1, NumVal n2) -> Right . NumVal $ func n1 n2
          (a, b)                 -> opError "number" op a b
        (_, Just func, _) -> case (val1, val2) of
          (NumVal n1, NumVal n2) -> Right . BoolVal $ func n1 n2
          (a, b)                 -> opError "number" op a b
        (_, _, Just func) -> case (val1, val2) of
          (BoolVal b1, BoolVal b2) -> Right . BoolVal $ func b1 b2
          (a, b)                   -> opError "boolean value" op a b
        _ -> invalidOpError op
  where
    wrapVal1 = valueOf expr1 env
    wrapVal2 = valueOf expr2 env
    opError typeName op a b = Left $ unlines
      [ "Operands of binary ", show op, " operator "
      , "should both be ", typeName, "s, but got: "
      , show a, " and ", show b
      ]

invalidOpError op = error $ "Invalid operator: " `mappend` show op

evalUnaryOpExpr :: UnaryOp
                -> Expression
                -> Environment
                -> Either String ExpressedValue
evalUnaryOpExpr op expr env =
  case valueOf expr env of
    msg@(Left _) -> msg
    (Right val) ->
      case ( lookup op unaryNumToNumOpMap
           , lookup op unaryNumToBoolOpMap
           , lookup op unaryBoolOpMap
           ) of
        (Just func, _, _) -> case val of
          (NumVal n) -> Right . NumVal $ func n
          _          -> opError "number" op val
        (_, Just func, _) -> case val of
          (NumVal n) -> Right . BoolVal $ func n
          _          -> opError "number" op val
        (_, _, Just func) -> case val of
          (BoolVal b) -> Right . BoolVal $ func b
          _           -> opError "boolean value" op val
        _ -> invalidOpError op
  where
    opError typeName op val = Left $ unlines
      [ "Operand of ", show op , " operator "
      , "should be ", typeName, ", but got: "
      , show val
      ]
