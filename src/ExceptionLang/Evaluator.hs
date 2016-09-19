module ExceptionLang.Evaluator
( valueOf
, run
, eval
, evalProgram
) where

import           Control.Applicative  ((<|>))
import           Control.Arrow        (second)
import           Control.Monad        ((>=>))
import           ExceptionLang.Data
import           ExceptionLang.Parser

type EvaluateResult = Try ExpressedValue

applyCont :: Continuation -> ExpressedValue -> EvaluateResult
applyCont EndCont val = return val
applyCont (UnaryOpCont func cont) val = func val >>= applyCont cont
applyCont (BinOpCont1 func expr2 env cont) val1 =
  valueOf expr2 env (BinOpCont2 func val1 cont)
applyCont (BinOpCont2 func val1 cont) val2 = func val1 val2 >>= applyCont cont
applyCont (IfCont thenE elseE env cont) val = do
  bool <- unpackBool "Predicate of if expression" val
  let body = if bool then thenE else elseE
  valueOf body env cont
applyCont (LetCont [] names valsAcc body env cont) val = do
  pairs <- safeZip names (reverse (val : valsAcc))
  let binds = fmap (second exprToDeno) pairs
  valueOf body (extendMany binds env) cont
applyCont (LetCont (expr : exprs) names valsAcc body env cont) val =
  valueOf expr env (LetCont exprs names (val : valsAcc) body env cont)
applyCont (RatorCont [] env cont) val = do
  proc <- unpackProc "Operator of call expression" val
  applyProcedure proc [] cont
applyCont (RatorCont (rand : rands) env cont) val =
  valueOf rand env (RandCont rands val [] env cont)
applyCont (RandCont [] rator randValsAcc env cont) val = do
  proc <- unpackProc "Operator of call expression" rator
  let rands = fmap exprToDeno (reverse (val : randValsAcc))
  applyProcedure proc rands cont
applyCont (RandCont (randExpr : randExprs) rator randValsAcc env cont) val =
  valueOf randExpr env (RandCont randExprs rator (val : randValsAcc) env cont)
applyCont (TryCont _ _ _ cont) val = applyCont cont val
applyCont (RaiseCont cont) val = handleException val cont

handleException :: ExpressedValue -> Continuation -> EvaluateResult
handleException val EndCont =
  throwError $ "Unhandle exception: " `mappend` show val
handleException val (TryCont ex handler env cont) =
  valueOf handler (extend ex (exprToDeno val) env) cont
handleException val (UnaryOpCont _ cont) = handleException val cont
handleException val (BinOpCont1 _ _ _ cont) = handleException val cont
handleException val (BinOpCont2 _ _ cont) = handleException val cont
handleException val (IfCont _ _ _ cont) = handleException val cont
handleException val (LetCont _ _ _ _ _ cont) = handleException val cont
handleException val (RatorCont _ _ cont) = handleException val cont
handleException val (RandCont _ _ _ _ cont) = handleException val cont
handleException val (RaiseCont cont) = handleException val cont

throwError :: String -> Try a
throwError = Left

liftMaybe :: String -> Maybe a -> Either String a
liftMaybe _ (Just x) = Right x
liftMaybe y Nothing  = throwError y

run :: String -> EvaluateResult
run input = parseProgram input >>= evalProgram

eval :: Expression -> Continuation -> EvaluateResult
eval expr = valueOf expr empty

evalProgram :: Program -> EvaluateResult
evalProgram (Prog expr) = eval expr EndCont

valueOf :: Expression -> Environment -> Continuation -> EvaluateResult
valueOf (ConstExpr x) _ cont             = evalConstExpr x cont
valueOf (VarExpr var) env cont           = evalVarExpr var env cont
valueOf (ProcExpr params body) env cont  = evalProcExpr params body env cont
valueOf (LetRecExpr procs body) env cont = evalLetRecExpr procs body env cont
valueOf (LetExpr binds body) env cont    = evalLetExpr binds body env cont
valueOf (UnaryOpExpr op e) env cont      = evalUnaryOpExpr op e env cont
valueOf (IfExpr e1 e2 e3) env cont       = evalIfExpr e1 e2 e3 env cont
valueOf (BinOpExpr op e1 e2) env cont    = evalBinOpExpr op e1 e2 env cont
valueOf (CallExpr rator rand) env cont   = evalCallExpr rator rand env cont
valueOf (TryExpr e1 ex e2) env cont      = evalTryExpr e1 ex e2 env cont
valueOf (RaiseExpr expr) env cont        = evalRaiseExpr expr env cont

exprToDeno :: ExpressedValue -> DenotedValue
exprToDeno (ExprNum n)  = DenoNum n
exprToDeno (ExprBool b) = DenoBool b
exprToDeno (ExprProc p) = DenoProc p

denoToExpr :: DenotedValue -> ExpressedValue
denoToExpr (DenoNum n)  = ExprNum n
denoToExpr (DenoBool b) = ExprBool b
denoToExpr (DenoProc p) = ExprProc p

evalConstExpr :: ExpressedValue -> Continuation -> EvaluateResult
evalConstExpr val cont = applyCont cont val

evalVarExpr :: String -> Environment -> Continuation -> EvaluateResult
evalVarExpr var env cont = do
  denoVal <- liftMaybe ("Not in scope: " `mappend` var) (apply env var)
  let exprVal = denoToExpr denoVal
  applyCont cont exprVal

evalLetRecExpr :: [(String, [String], Expression)]
               -> Expression -> Environment -> Continuation
               -> EvaluateResult
evalLetRecExpr procsSubUnits recBody env =
  valueOf recBody $ extendRecMany procsSubUnits env

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
unpackNum caller notNum = throwError $ concat [
  caller, ": Unpacking a not number value: ", show notNum ]

unpackBool :: String -> ExpressedValue -> Try Bool
unpackBool _ (ExprBool b) = return b
unpackBool caller notBool = throwError $ concat [
  caller, ": Unpacking a not boolean value: ", show notBool ]

tryFind :: Eq a => String -> a -> [(a, b)] -> Try b
tryFind err x pairs = liftMaybe err (lookup x pairs)

tryFindOp :: (Eq a, Show a) => a -> [(a, b)] -> Try b
tryFindOp op = tryFind ("Unknown operator: " `mappend` show op) op

evalBinOpExpr :: BinOp -> Expression -> Expression -> Environment
              -> Continuation
              -> EvaluateResult
evalBinOpExpr op expr1 expr2 env cont = do
  func <- n2nFunc <|> n2bFunc <|> b2bFunc
  valueOf expr1 env (BinOpCont1 func expr2 env cont)
  where
    findOpFrom = tryFindOp op
    unpackN = unpackNum $ "binary operation " `mappend` show op
    unpackB = unpackBool $ "binary operation " `mappend` show op
    n2nFunc :: Try (ExpressedValue -> ExpressedValue -> Try ExpressedValue)
    n2nFunc = do
      func <- findOpFrom binNumToNumOpMap
      return (\val1 val2 -> do
                n1 <- unpackN val1
                n2 <- unpackN val2
                return . ExprNum $ func n1 n2)
    n2bFunc :: Try (ExpressedValue -> ExpressedValue -> Try ExpressedValue)
    n2bFunc = do
      func <- findOpFrom binNumToBoolOpMap
      return (\val1 val2 -> do
                n1 <- unpackN val1
                n2 <- unpackN val2
                return . ExprBool $ func n1 n2)
    b2bFunc :: Try (ExpressedValue -> ExpressedValue -> Try ExpressedValue)
    b2bFunc = do
      func <- findOpFrom binBoolOpMap
      return (\val1 val2 -> do
                b1 <- unpackB val1
                b2 <- unpackB val2
                return . ExprBool $ func b1 b2)

evalUnaryOpExpr :: UnaryOp -> Expression -> Environment
                -> Continuation
                -> EvaluateResult
evalUnaryOpExpr op expr env cont = do
  func <- n2nFunc <|> n2bFunc <|> b2bFunc
  valueOf expr env (UnaryOpCont func cont)
  where
    findOpFrom = tryFindOp op
    unpackN = unpackNum $ "unary operation " `mappend` show op
    unpackB = unpackBool $ "unary operation " `mappend` show op
    n2nFunc :: Try (ExpressedValue -> Try ExpressedValue)
    n2nFunc = do
      func <- findOpFrom unaryNumToNumOpMap
      return (unpackN >=> (return . ExprNum . func))
    n2bFunc :: Try (ExpressedValue -> Try ExpressedValue)
    n2bFunc = do
      func <- findOpFrom unaryNumToBoolOpMap
      return (unpackN >=> (return . ExprBool . func))
    b2bFunc :: Try (ExpressedValue -> Try ExpressedValue)
    b2bFunc = do
      func <- findOpFrom unaryBoolOpMap
      return (unpackB >=> (return . ExprBool . func))

evalLetExpr :: [(String, Expression)] -> Expression -> Environment
            -> Continuation
            -> EvaluateResult
evalLetExpr [] body env cont = valueOf body env cont
evalLetExpr pairs body env cont =
  valueOf expr env (LetCont exprs names [] body env cont)
  where
    (names, expr : exprs) = unzip pairs

evalProcExpr :: [String] -> Expression -> Environment -> Continuation
             -> EvaluateResult
evalProcExpr params body env cont =
  applyCont cont . ExprProc $ Procedure params body env

evalIfExpr :: Expression -> Expression -> Expression -> Environment
           -> Continuation
           -> EvaluateResult
evalIfExpr ifE thenE elseE env cont =
  valueOf ifE env (IfCont thenE elseE env cont)

unpackProc :: String -> ExpressedValue -> Try Procedure
unpackProc _ (ExprProc proc) = return proc
unpackProc caller notProc = throwError $ concat [
  caller, ": Unpacking a not procedure value: ", show notProc ]

evalCallExpr :: Expression -> [Expression] -> Environment -> Continuation
             -> EvaluateResult
evalCallExpr rator rands env cont =
  valueOf rator env (RatorCont rands env cont)

safeZip :: [a] -> [b] -> Try [(a, b)]
safeZip [] []             = return []
safeZip (a : as) (b : bs) = ((a, b) :) <$> safeZip as bs
safeZip _ _               = throwError "Unmatched parameters and arguements!"

applyProcedure :: Procedure -> [DenotedValue] -> Continuation
               -> EvaluateResult
applyProcedure (Procedure params body savedEnv) rands cont = do
  argPairs <- safeZip params rands
  valueOf body (extendMany argPairs savedEnv) cont

evalTryExpr :: Expression -> String -> Expression -> Environment
            -> Continuation
            -> EvaluateResult
evalTryExpr body ex handler env cont =
  valueOf body env (TryCont ex handler env cont)

evalRaiseExpr :: Expression -> Environment -> Continuation -> EvaluateResult
evalRaiseExpr expr env cont = undefined

