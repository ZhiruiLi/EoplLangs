module LetRecLang.Evaluator
( valueOf
, run
, eval
, evalProgram
) where

import           Control.Applicative ((<|>))
import           Control.Arrow       (second)
import           LetRecLang.Data
import           LetRecLang.Parser

type EvaluateResult = Try ExpressedValue

liftMaybe :: LangError -> Maybe a -> Try a
liftMaybe _ (Just x) = return x
liftMaybe y Nothing  = throwError y

run :: String -> EvaluateResult
run input = parseProgram input >>= evalProgram

eval :: Expression -> EvaluateResult
eval = flip valueOf empty

evalProgram :: Program -> EvaluateResult
evalProgram (Prog expr) = eval expr

valueOf :: Expression -> Environment -> EvaluateResult
valueOf (ConstExpr x) _                 = evalConstExpr x
valueOf (VarExpr var) env               = evalVarExpr var env
valueOf (LetRecExpr procs recBody) env  = evalLetRecExpr procs recBody env
valueOf (BinOpExpr op expr1 expr2) env  = evalBinOpExpr op expr1 expr2 env
valueOf (UnaryOpExpr op expr) env       = evalUnaryOpExpr op expr env
valueOf (CondExpr pairs) env            = evalCondExpr pairs env
valueOf (LetExpr bindings body) env     = evalLetExpr bindings body env
valueOf (LetStarExpr bindings body) env = evalLetStarExpr bindings body env
valueOf (ProcExpr params body) env      = evalProcExpr params body env
valueOf (CallExpr rator rand) env       = evalCallExpr rator rand env

evalConstExpr :: ExpressedValue -> EvaluateResult
evalConstExpr = Right

exprToDeno :: ExpressedValue -> DenotedValue
exprToDeno (ExprNum n)  = DenoNum n
exprToDeno (ExprBool b) = DenoBool b
exprToDeno (ExprProc p) = DenoProc p

denoToExpr :: DenotedValue -> ExpressedValue
denoToExpr (DenoNum n)  = ExprNum n
denoToExpr (DenoBool b) = ExprBool b
denoToExpr (DenoProc p) = ExprProc p

evalVarExpr :: String -> Environment -> EvaluateResult
evalVarExpr var env =
  denoToExpr <$> liftMaybe (UnboundVar var) (apply env var)

evalLetRecExpr :: [(String, [String], Expression)] -> Expression -> Environment
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

tryFind :: Eq a => LangError -> a -> [(a, b)] -> Try b
tryFind err x pairs = liftMaybe err (lookup x pairs)

tryFindOp :: (Eq a, Show a) => a -> [(a, b)] -> Try b
tryFindOp op = tryFind (UnknownOperator $ show op) op

binOpConverter :: (ExpressedValue -> Try a)
               -> (ExpressedValue -> Try b)
               -> (c -> ExpressedValue)
               -> (a -> b -> c)
               -> (ExpressedValue -> ExpressedValue -> EvaluateResult)
binOpConverter unpack1 unpack2 trans func val1 val2 = do
  va <- unpack1 val1
  vb <- unpack2 val2
  return . trans $ func va vb

binOps :: [(BinOp, ExpressedValue -> ExpressedValue -> EvaluateResult)]
binOps = concat [binNum2Num, binNum2Bool, binBool2Bool]
  where
    n2nTrans = binOpConverter unpackNum unpackNum ExprNum
    binNum2Num = fmap (second n2nTrans) binNumToNumOpMap
    n2bTrans = binOpConverter unpackNum unpackNum ExprBool
    binNum2Bool = fmap (second n2bTrans) binNumToBoolOpMap
    b2bTrans = binOpConverter unpackBool unpackBool ExprBool
    binBool2Bool = fmap (second b2bTrans) binBoolOpMap

unaryOpConverter :: (ExpressedValue -> Try a)
                 -> (b -> ExpressedValue)
                 -> (a -> b)
                 -> (ExpressedValue -> EvaluateResult)
unaryOpConverter unpack trans func val = do
  va <- unpack val
  return . trans $ func va

unaryOps :: [(UnaryOp, ExpressedValue -> EvaluateResult)]
unaryOps = concat [unaryNum2Num, unaryNum2Bool, unaryBool2Bool]
  where
    n2nTrans = unaryOpConverter unpackNum ExprNum
    unaryNum2Num = fmap (second n2nTrans) unaryNumToNumOpMap
    n2bTrans = unaryOpConverter unpackNum ExprBool
    unaryNum2Bool = fmap (second n2bTrans) unaryNumToBoolOpMap
    b2bTrans = unaryOpConverter unpackBool ExprBool
    unaryBool2Bool = fmap (second b2bTrans) unaryBoolOpMap

evalBinOpExpr :: BinOp -> Expression -> Expression -> Environment
              -> EvaluateResult
evalBinOpExpr op expr1 expr2 env = do
  func <- tryFindOp op binOps
  v1 <- valueOf expr1 env
  v2 <- valueOf expr2 env
  func v1 v2

evalUnaryOpExpr :: UnaryOp -> Expression -> Environment
                -> EvaluateResult
evalUnaryOpExpr op expr env = do
  func <- tryFindOp op unaryOps
  v <- valueOf expr env
  func v

evalCondExpr :: [(Expression, Expression)] -> Environment -> EvaluateResult
evalCondExpr [] _ = throwError $ DefaultError "No predicate is true."
evalCondExpr ((e1, e2):pairs) env = do
  val <- valueOf e1 env
  bool <- unpackBool val
  if bool then valueOf e2 env else evalCondExpr pairs env

evalLetExpr :: [(String, Expression)] -> Expression -> Environment
            -> EvaluateResult
evalLetExpr bindings body env = do
  bindVals <- evaledBindings
  let bindDenoVals = fmap (second exprToDeno) bindVals
  valueOf body $ extendMany bindDenoVals env
  where
    func maybeBindVals (name, expr) = do
      pairs <- maybeBindVals
      val <- valueOf expr env
      return $ (name, val):pairs
    evaledBindings = do
      pairs <- foldl func (return []) bindings
      return $ reverse pairs

evalLetStarExpr :: [(String, Expression)] -> Expression -> Environment
                -> EvaluateResult
evalLetStarExpr [] body env = valueOf body env
evalLetStarExpr ((var, expr):pairs) body env = do
  val <- valueOf expr env
  evalLetStarExpr pairs body (extend var (exprToDeno val) env)

evalProcExpr :: [String] -> Expression -> Environment -> EvaluateResult
evalProcExpr params body env = return . ExprProc $ Procedure params body env

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
      return $ arg : args
    maybeArgs :: Try [ExpressedValue]
    maybeArgs = reverse <$>
      foldl func (return []) (fmap (`valueOf` env) rand)
    applyProcedure :: Procedure -> [ExpressedValue] -> EvaluateResult
    applyProcedure (Procedure params body savedEnv) args = do
      pairs <- safeZip params args
      applyProcedure' pairs body savedEnv []
    applyProcedure' :: [(String, ExpressedValue)] -> Expression -> Environment
                    -> [String]
                    -> EvaluateResult
    applyProcedure' [] body env _ = valueOf body env
    applyProcedure' ((name, val) : pairs) body env names
      | name `elem` names =
          throwError . DefaultError $ "Parameter name conflict" `mappend` name
      | otherwise = let newEnv = extend name (exprToDeno val) env
                    in applyProcedure' pairs body newEnv (name : names)

safeZip :: [String] -> [ExpressedValue] -> Try [(String, ExpressedValue)]
safeZip names args = if nl == pl
  then return $ zip names args
  else throwError $ ArgNumMismatch (toInteger nl) args
  where (nl, pl) = (length names, length args)
