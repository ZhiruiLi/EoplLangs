module NamelessIntp.Evaluator
( valueOf
, run
, eval
, evalProgram
) where

import           Debug.Trace             (trace)
import           NamelessIntp.Data
import           NamelessIntp.Parser
import           NamelessIntp.Translator

type EvaluateResult = Try ExpressedValue

liftMaybe :: a -> Maybe b -> Either a b
liftMaybe _ (Just x) = Right x
liftMaybe y Nothing  = Left y

run :: String -> EvaluateResult
run input = do
  prog <- parseProgram input
  nameless <- translateProgram prog
  evalProgram nameless

eval :: NamelessExpression -> EvaluateResult
eval = flip valueOf empty

evalProgram :: NamelessProgram -> EvaluateResult
evalProgram (NamelessProg expr) = eval expr

valueOf :: NamelessExpression -> NamelessEnvironment -> EvaluateResult
valueOf (NamelessConstExpr n) _ = evalConstExpr n
valueOf (NamelessVarExpr addr) env = evalVarExpr addr env
valueOf (NamelessIfExpr ifE thenE elseE) env = evalIfExpr ifE thenE elseE env
valueOf (NamelessBinOpExpr op expr1 expr2) env = evalBinOpExpr op expr1 expr2 env
valueOf (NamelessUnaryOpExpr op expr) env = evalUnaryOpExpr op expr env
valueOf (NamelessLetExpr expr body) env = evalLetExpr expr body env
valueOf (NamelessProcExpr body) env = evalProcExpr body env
valueOf (NamelessCallExpr rator rand) env = evalCallExpr rator rand env
valueOf (NamelessCondExpr pairs) env = evalCondExpr pairs env
valueOf (NamelessLetRecExpr proc body) env = evalLetRecExpr proc body env

evalConstExpr :: Integer -> EvaluateResult
evalConstExpr n = Right $ ExprNum n

evalVarExpr :: Integer -> NamelessEnvironment -> EvaluateResult
evalVarExpr addr env = liftMaybe
  ("Invalid address: " `mappend` show addr)
  (apply env addr)

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
              -> NamelessExpression
              -> NamelessExpression
              -> NamelessEnvironment
              -> EvaluateResult
evalBinOpExpr op expr1 expr2 env = do
  val1 <- valueOf expr1 env
  val2 <- valueOf expr2 env
  case ( lookup op binNumToNumOpMap
       , lookup op binNumToBoolOpMap
       , lookup op binBoolOpMap
       ) of
    (Just func, _, _) -> case (val1, val2) of
      (ExprNum n1, ExprNum n2) -> Right . ExprNum $ func n1 n2
      (a, b)                   -> opError "number" op a b
    (_, Just func, _) -> case (val1, val2) of
      (ExprNum n1, ExprNum n2) -> Right . ExprBool $ func n1 n2
      (a, b)                   -> opError "number" op a b
    (_, _, Just func) -> case (val1, val2) of
      (ExprBool b1, ExprBool b2) -> Right . ExprBool $ func b1 b2
      (a, b)                     -> opError "boolean value" op a b
    _ -> invalidOpError op
  where
    opError typeName op a b = Left $ concat
      [ "Operands of binary ", show op, " operator "
      , "should both be ", typeName, "s, but got: "
      , show a, " and ", show b
      ]

invalidOpError op = error $ "Invalid operator: " `mappend` show op

evalUnaryOpExpr :: UnaryOp
                -> NamelessExpression
                -> NamelessEnvironment
                -> EvaluateResult
evalUnaryOpExpr op expr env = do
  val <- valueOf expr env
  case ( lookup op unaryNumToNumOpMap
       , lookup op unaryNumToBoolOpMap
       , lookup op unaryBoolOpMap
       ) of
    (Just func, _, _) -> case val of
      (ExprNum n) -> Right . ExprNum $ func n
      _           -> opError "number" op val
    (_, Just func, _) -> case val of
      (ExprNum n) -> Right . ExprBool $ func n
      _           -> opError "number" op val
    (_, _, Just func) -> case val of
      (ExprBool b) -> Right . ExprBool $ func b
      _            -> opError "boolean value" op val
    _ -> invalidOpError op
  where
    opError typeName op val = Left $ concat
      [ "Operand of ", show op , " operator "
      , "should be ", typeName, ", but got: "
      , show val
      ]

evalIfExpr :: NamelessExpression
           -> NamelessExpression
           -> NamelessExpression
           -> NamelessEnvironment
           -> EvaluateResult
evalIfExpr ifE thenE elseE env = do
  val <- valueOf ifE env
  b <- checkBool val "Predicate of if expression"
  valueOf (if b then thenE else elseE) env


checkBool (ExprBool b) _ = return b
checkBool notBool name = Left $ concat
  [ name, " should be boolean, but got: ", show notBool ]

evalProcExpr :: NamelessExpression -> NamelessEnvironment -> EvaluateResult
evalProcExpr body env = return . ExprProc $ NamelessProcedure body env

evalLetExpr :: NamelessExpression
            -> NamelessExpression
            -> NamelessEnvironment
            -> EvaluateResult
evalLetExpr expr body env = do
  val <- valueOf expr env
  valueOf body (extend val env)

evalCallExpr :: NamelessExpression
             -> NamelessExpression
             -> NamelessEnvironment
             -> EvaluateResult
evalCallExpr rator rand env = do
  ratorVal <- valueOf rator env
  proc <- checkProc ratorVal
  randVal <- valueOf rand env
  applyProcedure proc randVal
  where
    checkProc (ExprProc proc) = Right proc
    checkProc noProc = Left $
      "Operator of call expression should be procedure, "
      `mappend` "but got: " `mappend` show noProc
    applyProcedure (NamelessProcedure body savedEnv) rand =
      valueOf body (extend rand savedEnv)

evalCondExpr :: [(NamelessExpression, NamelessExpression)]
             -> NamelessEnvironment
             -> EvaluateResult
evalCondExpr ((e1, e2):pairs) env = do
  v1 <- valueOf e1 env
  b <- checkBool v1 "Predicate of cond expression"
  if b then valueOf e2 env else evalCondExpr pairs env
evalCondExpr [] _ = Left "No predicate is true"

evalLetRecExpr :: NamelessExpression -> NamelessExpression
               -> NamelessEnvironment
               -> EvaluateResult
evalLetRecExpr procBody body env = valueOf body (extend proc env)
  where
    proc = ExprProc $ NamelessProcedure procBody (extend proc env)
