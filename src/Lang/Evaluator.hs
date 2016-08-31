module Lang.Evaluator
( valueOf
, run
, eval
, evalProgram
) where

import           Lang.Data.Environment
import           Lang.Data.ExpressedValue
import           Lang.Data.Expression
import           Lang.Data.Program
import           Lang.Parser

run :: String -> ExpressedValue
run input = case parseProgram input of
  Left msg   -> error msg
  Right prog -> case evalProgram prog of
    Left msg -> error msg
    Right v  -> v

eval :: Expression -> Either String ExpressedValue
eval = flip valueOf empty

evalProgram :: Program -> Either String ExpressedValue
evalProgram (Program expr) = eval expr

valueOf :: Expression -> Environment -> Either String ExpressedValue
valueOf (ConstExpr n) _   = Right $ NumVal n
valueOf (VarExpr var) env = case applySafe env var of
  Nothing  -> Left $ "Not in scope: " `mappend` var
  Just val -> Right val
valueOf (DiffExpr expr1 expr2) env =
  case (val1, val2) of
    (Right (NumVal n1), Right (NumVal n2)) -> Right . NumVal $ n1 - n2
    (msg@(Left _), _) -> msg
    (_, msg@(Left _)) -> msg
    (a, b) -> Left $
      "Operands of diff expression should both be numbers, but got: " `mappend`
      show a `mappend` " and " `mappend` show b
  where
    val1 = valueOf expr1 env
    val2 = valueOf expr2 env
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

