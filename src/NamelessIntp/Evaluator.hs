module NamelessIntp.Evaluator
( valueOf
, run
, eval
, evalProgram
) where

import           Control.Applicative     ((<|>))
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

unpackNum :: String -> ExpressedValue -> Try Integer
unpackNum _ (ExprNum n) = return n
unpackNum caller notNum = Left $ concat [
  caller, ": Unpacking a not number value: ", show notNum ]

unpackBool :: String -> ExpressedValue -> Try Bool
unpackBool _ (ExprBool b) = return b
unpackBool caller notBool = Left $ concat [
  caller, ": Unpacking a not boolean value: ", show notBool ]

tryFind :: Eq a => String -> a -> [(a, b)] -> Try b
tryFind err x pairs = liftMaybe err (lookup x pairs)

tryFindOp :: (Eq a, Show a) => a -> [(a, b)] -> Try b
tryFindOp op = tryFind ("Unknown operator: " ++ show op) op

evalBinOpExpr :: BinOp -> NamelessExpression -> NamelessExpression
              -> NamelessEnvironment
              -> EvaluateResult
evalBinOpExpr op expr1 expr2 env = do
  v1 <- valueOf expr1 env
  v2 <- valueOf expr2 env
  numToNum v1 v2 <|> numToBool v1 v2  <|> boolToBool v1 v2
  where
    findOpFrom = tryFindOp op
    unpackN = unpackNum $ "binary operation " ++ show op
    unpackB = unpackBool $ "binary operation " ++ show op
    numToNum :: ExpressedValue -> ExpressedValue -> EvaluateResult
    numToNum val1 val2 = do
      func <- findOpFrom binNumToNumOpMap
      n1 <- unpackN val1
      n2 <- unpackN val2
      return . ExprNum $ func n1 n2
    numToBool :: ExpressedValue -> ExpressedValue -> EvaluateResult
    numToBool val1 val2 = do
      func <- findOpFrom binNumToBoolOpMap
      n1 <- unpackN val1
      n2 <- unpackN val2
      return . ExprBool $ func n1 n2
    boolToBool :: ExpressedValue -> ExpressedValue -> EvaluateResult
    boolToBool val1 val2 = do
      func <- findOpFrom binBoolOpMap
      b1 <- unpackB val1
      b2 <- unpackB val2
      return . ExprBool $ func b1 b2

evalUnaryOpExpr :: UnaryOp -> NamelessExpression
                -> NamelessEnvironment
                -> EvaluateResult
evalUnaryOpExpr op expr env = do
  v <- valueOf expr env
  numToNum v <|> numToBool v <|> boolToBool v
  where
    findOpFrom = tryFindOp op
    unpackN = unpackNum $ "unary operation " ++ show op
    unpackB = unpackBool $ "unary operation " ++ show op
    numToNum :: ExpressedValue -> EvaluateResult
    numToNum val = do
      func <- findOpFrom unaryNumToNumOpMap
      n <- unpackN val
      return . ExprNum $ func n
    numToBool :: ExpressedValue -> EvaluateResult
    numToBool val = do
      func <- findOpFrom unaryNumToBoolOpMap
      n <- unpackN val
      return . ExprBool $ func n
    boolToBool :: ExpressedValue -> EvaluateResult
    boolToBool val = do
      func <- findOpFrom unaryBoolOpMap
      b <- unpackB val
      return . ExprBool $ func b

evalIfExpr :: NamelessExpression
           -> NamelessExpression
           -> NamelessExpression
           -> NamelessEnvironment
           -> EvaluateResult
evalIfExpr ifE thenE elseE env = do
  val <- valueOf ifE env
  b <- unpackBool "Predicate of if expression" val
  valueOf (if b then thenE else elseE) env

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
  proc <- unpackProc ratorVal
  randVal <- valueOf rand env
  applyProcedure proc randVal
  where
    unpackProc (ExprProc proc) = Right proc
    unpackProc noProc = Left $
      "Operator of call expression should be procedure, "
      `mappend` "but got: " `mappend` show noProc
    applyProcedure (NamelessProcedure body savedEnv) rand =
      valueOf body (extend rand savedEnv)

evalCondExpr :: [(NamelessExpression, NamelessExpression)]
             -> NamelessEnvironment
             -> EvaluateResult
evalCondExpr ((e1, e2):pairs) env = do
  v1 <- valueOf e1 env
  b <- unpackBool "Predicate of cond expression" v1
  if b then valueOf e2 env else evalCondExpr pairs env
evalCondExpr [] _ = Left "No predicate is true"

evalLetRecExpr :: NamelessExpression -> NamelessExpression
               -> NamelessEnvironment
               -> EvaluateResult
evalLetRecExpr procBody body env = valueOf body (extend proc env)
  where
    proc = ExprProc $ NamelessProcedure procBody (extend proc env)
