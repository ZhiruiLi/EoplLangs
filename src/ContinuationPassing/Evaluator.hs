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
eval expr = valueOf expr empty

evalProgram :: Program -> EvaluateResult
evalProgram (Prog expr) = eval expr endCont

valueOf :: Expression -> Environment -> Continuation -> EvaluateResult
valueOf (ConstExpr x) _ cont = evalConstExpr x cont
valueOf (VarExpr var) env cont = evalVarExpr var env cont
valueOf (ProcExpr params body) env cont = evalProcExpr params body env cont
valueOf (LetRecExpr procs body) env cont = evalLetRecExpr procs body env cont
valueOf (LetExpr binds body) env cont = evalLetExpr binds body env cont
valueOf (UnaryOpExpr op e) env cont = evalUnaryOpExpr op e env cont
valueOf (IfExpr e1 e2 e3) env cont = evalIfExpr e1 e2 e3 env cont
valueOf (BinOpExpr op e1 e2) env cont  = evalBinOpExpr op e1 e2 env cont
valueOf (CallExpr rator rand) env cont = evalCallExpr rator rand env cont

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
  valueOf expr1 env (extendBinOp1Cont func expr2 env cont)
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
  valueOf expr env (extendUnaryOpCont func cont)
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
evalLetExpr pairs body env cont = evalLetExpr' pairs body env cont
  where
    evalLetExpr' :: [(String, Expression)] -> Expression -> Environment
                 -> Continuation
                 -> EvaluateResult
    evalLetExpr' [] body newEnv cont = valueOf body newEnv cont
    evalLetExpr' ((name, expr):pairs) body newEnv cont =
      valueOf expr env (extendCont func cont)
      where
        func :: ExpressedValue -> Try ExpressedValue
        func val = let env = extend name (exprToDeno val) newEnv
                   in evalLetExpr' pairs body env cont

evalProcExpr :: [String] -> Expression -> Environment -> Continuation
             -> EvaluateResult
evalProcExpr params body env cont =
  applyCont cont . ExprProc $ Procedure params body env

evalIfExpr :: Expression -> Expression -> Expression -> Environment
           -> Continuation
           -> EvaluateResult
evalIfExpr ifE thenE elseE env cont =
  valueOf ifE env (extendIfTestCont thenE elseE env cont)

unpackProc :: String -> ExpressedValue -> Try Procedure
unpackProc _ (ExprProc proc) = return proc
unpackProc caller notProc = throwError $ concat [
  caller, ": Unpacking a not procedure value: ", show notProc ]

evalCallExpr :: Expression -> [Expression] -> Environment -> Continuation
             -> EvaluateResult
evalCallExpr rator rands env cont =
  valueOf rator env (extendCheckRatorCont rands env cont)

extendCheckRatorCont :: [Expression] -> Environment -> Continuation
                     -> Continuation
extendCheckRatorCont randExprs env cont = Continuation $ \rator ->
  evalCallExprTail randExprs rator [] env cont

evalCallExprTail :: [Expression] -> ExpressedValue -> [ExpressedValue]
                 -> Environment -> Continuation
                 -> EvaluateResult
evalCallExprTail [] rator randsAcc env cont = do
  proc <- unpackProc "procedure call" rator
  applyProcedure proc (reverse randsAcc) env cont
evalCallExprTail (randE:randEs) rator randsAcc env cont =
  valueOf randE env (extendCallRandsCont randEs rator randsAcc env cont)

extendCallRandsCont :: [Expression] -> ExpressedValue -> [ExpressedValue]
                    -> Environment -> Continuation
                    -> Continuation
extendCallRandsCont randEs rator randsAcc env cont = undefined

applyProcedure :: Procedure -> [ExpressedValue] -> Environment -> Continuation
               -> EvaluateResult
applyProcedure p randsAcc env cont = undefined

{-
evalCallExpr :: Expression -> [Expression] -> Environment -> EvaluateResult
evalCallExpr rator rand env = do
  rator <- valueOf rator env
  proc <- unpackProc rator
  args <- maybeArgs
  applyProcedure proc args
  where
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

extendCont :: (ExpressedValue -> Try ExpressedValue) -> Continuation
              -> Continuation
extendCont func cont = Continuation $ \val -> do
  newVal <- func val
  applyCont cont newVal

extendUnaryOpCont :: (ExpressedValue -> Try ExpressedValue) -> Continuation
                  -> Continuation
extendUnaryOpCont func cont = Continuation $ \val -> do
  newVal <- func val
  applyCont cont newVal

extendIfTestCont :: Expression -> Expression -> Environment -> Continuation
                 -> Continuation
extendIfTestCont thenE elseE env cont = Continuation $ \val -> do
  bool <- unpackBool "Predicate of if expression" val
  let expr = if bool then thenE else elseE
  valueOf expr env cont

extendBinOp1Cont :: (ExpressedValue -> ExpressedValue -> Try ExpressedValue)
                 -> Expression -> Environment -> Continuation
                 -> Continuation
extendBinOp1Cont func expr2 env cont = Continuation $ \val1 ->
  valueOf expr2 env (extendBinOp2Cont func val1 cont)

extendBinOp2Cont :: (ExpressedValue -> ExpressedValue -> Try ExpressedValue)
                 -> ExpressedValue -> Continuation
                 -> Continuation
extendBinOp2Cont func val1 cont = Continuation $ \val2 -> do
  val <- func val1 val2
  applyCont cont val
