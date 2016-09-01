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
valueOf (BinOpExpr op expr1 expr2) env =
  evalBinOpExpr op expr1 expr2 env
valueOf (IsZeroExpr expr) env =
  case valueOf expr env of
    Right (NumVal n) -> Right . BoolVal $ n == 0
    Right x          -> Left $
      "Operand of zero? expression should be number, but got: " `mappend`
      show x
    left             -> left
valueOf (MinusExpr expr) env =
  case valueOf expr env of
    Right (NumVal n) -> Right . NumVal $ -n
    Right x          -> Left $
      "Operand of minus expression should be number, but got: " `mappend`
      show x
    left             -> left
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

boolOpMap :: [(Op, Bool -> Bool -> Bool)]
boolOpMap = []

numToNumOpMap :: [(Op, Integer -> Integer -> Integer)]
numToNumOpMap = [(Add, (+)), (Sub, (-)), (Mul, (*)), (Div, div)]

numToBoolOpMap :: [(Op, Integer -> Integer -> Bool)]
numToBoolOpMap = [(Gt, (>)), (Le, (<)), (Eq, (==))]

evalBinOpExpr op expr1 expr2 env =
  case (wrapVal1, wrapVal2) of
    (msg@(Left _), _) -> msg
    (_, msg@(Left _)) -> msg
    (Right val1, Right val2) ->
      case ( lookup op numToNumOpMap
           , lookup op numToBoolOpMap
           , lookup op boolOpMap ) of
        (Just func, _, _) -> case (val1, val2) of
          (NumVal n1, NumVal n2) -> Right . NumVal $ func n1 n2
          (a, b)                 -> numError a b
        (_, Just func, _) -> case (val1, val2) of
          (NumVal n1, NumVal n2) -> Right . BoolVal $ func n1 n2
          (a, b)                 -> numError a b
        (_, _, Just func) -> case (val1, val2) of
          (BoolVal b1, BoolVal b2) -> Right . BoolVal $ func b1 b2
          (a, b)                   -> boolError a b
        _ -> invalidError op
  where
    wrapVal1 = valueOf expr1 env
    wrapVal2 = valueOf expr2 env
    opError typeName a b = Left $ unlines
      [ "Operands of binary ", typeName, " operate expression "
      , "should both be ", typeName, "s, but got: "
      , show a, " and ", show b
      ]
    numError = opError "number"
    boolError = opError "boolean value"
    invalidError op = error $ "Invalid operator: " `mappend` show op
