module ContinuationPassing.Evaluator
( valueOf
, run
, eval
, evalProgram
) where

import           ContinuationPassing.Data
import           ContinuationPassing.Parser
import           Control.Applicative        ((<|>))
import           Control.Arrow              (second)
import           Control.Monad              ((>=>))

type EvaluateResult = Try ExpressedValue

throwError :: String -> Try a
throwError = Left

liftMaybe :: String -> Maybe a -> Either String a
liftMaybe _ (Just x) = Right x
liftMaybe y Nothing  = throwError y

run :: String -> EvaluateResult
run input = parseProgram input >>= evalProgram

eval :: Expression -> Continuation -> EvaluateResult
eval expr cont = valueOf expr empty cont >>= applyCont cont

evalProgram :: Program -> EvaluateResult
evalProgram (Prog expr) = eval expr endCont

valueOf :: Expression -> Environment -> Continuation -> EvaluateResult
valueOf (ConstExpr x) _ cont            = evalConstExpr x cont
valueOf (VarExpr var) env cont          = evalVarExpr var env cont
valueOf (ProcExpr params body) env cont = evalProcExpr params body env cont
valueOf (LetExpr binds body) env cont   = evalLetExpr binds body env cont
valueOf (UnaryOpExpr op e) env cont     = evalUnaryOpExpr op e env cont
valueOf (IfExpr e1 e2 e3) env cont      = evalIfExpr e1 e2 e3 env cont
-- valueOf (LetRecExpr procs body) env  cont = evalLetRecExpr procs body env cont
-- valueOf (BinOpExpr op e1 e2) env cont  = evalBinOpExpr op expr1 expr2 env cont
-- valueOf (CallExpr rator rand) env cont = evalCallExpr rator rand env cont

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
tryFindOp op = tryFind ("Unknown operator: " ++ show op) op
{-
evalBinOpExpr :: BinOp -> Expression -> Expression -> Environment
              -> Continuation
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
-}

evalUnaryOpExpr :: UnaryOp -> Expression -> Environment
                -> Continuation
                -> EvaluateResult
evalUnaryOpExpr op expr env cont = do
  func <- n2nFunc <|> n2bFunc <|> b2bFunc
  valueOf expr env (extendCont func cont)
  where
    findOpFrom = tryFindOp op
    unpackN = unpackNum $ "unary operation " ++ show op
    unpackB = unpackBool $ "unary operation " ++ show op
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
evalLetExpr pairs body env cont = evalLetExpr' pairs body env cont
  where
    evalLetExpr' :: [(String, Expression)] -> Expression -> Environment
                 -> Continuation
                 -> EvaluateResult
    evalLetExpr' ((name, expr):pairs) body newEnv cont =
      valueOf expr env (extendCont func cont)
      where
        func :: ExpressedValue -> Try ExpressedValue
        func val = evalLetExpr' pairs body
                                (extend name (exprToDeno val) newEnv)
                                cont

evalProcExpr :: [String] -> Expression -> Environment -> Continuation
             -> EvaluateResult
evalProcExpr params body env cont =
  applyCont cont . ExprProc $ Procedure params body env

evalIfExpr :: Expression -> Expression -> Expression -> Environment
           -> Continuation
           -> EvaluateResult
evalIfExpr ifE thenE elseE env cont = undefined

{-
evalCallExpr :: Expression -> [Expression] -> Environment -> EvaluateResult
evalCallExpr rator rand env = do
  rator <- valueOf rator env
  proc <- unpackProc rator
  args <- maybeArgs
  applyProcedure proc args
  where
    unpackProc :: ExpressedValue -> Try Procedure
    unpackProc (ExprProc proc) = Right proc
    unpackProc noProc = throwError $
      "Operator of call expression should be procedure, but got: "
      `mappend` show noProc
    func :: Try [ExpressedValue] -> Try ExpressedValue -> Try [ExpressedValue]
    func maybeArgs maybeArg = do
      args <- maybeArgs
      arg <- maybeArg
      return $ arg:args
    maybeArgs :: Try [ExpressedValue]
    maybeArgs = reverse <$>
      foldl func (return []) (fmap (`valueOf` env) rand)
    applyProcedure :: Procedure -> [ExpressedValue] -> EvaluateResult
    applyProcedure (Procedure params body savedEnv) args =
      applyProcedure' params body savedEnv args []
    applyProcedure' :: [String] -> Expression -> Environment
                    -> [ExpressedValue] -> [String]
                    -> EvaluateResult
    applyProcedure' [] body env [] _ = valueOf body env
    applyProcedure' params _ _ [] _ =
      throwError $ "Too many parameters: " `mappend` show params
    applyProcedure' [] _ _ args _ =
      throwError $ "Too many arguments: " `mappend` show args
    applyProcedure' (p:ps) body env (a:as) usedParams =
      if p `elem` usedParams
        then throwError $ "Parameter name conflict: " `mappend` p
        else applyProcedure' ps body (extend p (exprToDeno a) env)
                             as (p:usedParams)
-}
