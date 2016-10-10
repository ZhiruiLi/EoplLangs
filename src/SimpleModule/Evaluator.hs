module SimpleModule.Evaluator
( valueOf
, run
, eval
, evalProgram
) where

import           Control.Applicative      ((<|>))
import           Control.Arrow            (second)
import           Control.Monad.Except
import           Data.Maybe               (fromMaybe)
import           SimpleModule.Data
import           SimpleModule.Parser
import           SimpleModule.TypeChecker (typeOfExpression)

type EvaluateResult = Try ExpressedValue

applyForce :: GeneralEnv a -> String -> a
applyForce env var = case apply env var of
  Right x -> x
  Left _  -> error $ concat [ "Var ", var, " is not in scope." ]

liftMaybe :: LangError -> Maybe a -> Try a
liftMaybe _ (Just x) = return x
liftMaybe y Nothing  = throwError y

forceEnvTry :: EnvTry a -> a
forceEnvTry (Left e)  = error $ show e
forceEnvTry (Right x) = x

run :: String -> EvaluateResult
run input = parseProgram input >>= evalProgram

liftTypeError :: Either TypeError a -> Try a
liftTypeError (Right x)  = return x
liftTypeError (Left err) = throwError (TypeCheckerError err)

eval :: [ModuleDef] -> Expression -> EvaluateResult
eval defs expr = do
  liftTypeError (typeOfExpression defs expr)
  env <- addModuleDefs defs empty
  valueOf expr env

addModuleDefs :: [ModuleDef] -> Environment -> Try Environment
addModuleDefs [] env = return env
addModuleDefs (ModuleDef name iface body : ds) env = do
  newEnv <- addModuleBody name body env
  pairs <- findModuleIface name iface newEnv
  addModuleDefs ds (addNamed name pairs env)

addModuleBody :: String -> ModuleBody -> Environment -> Try Environment
addModuleBody name (ModuleBody defs) = addModuleBody' defs
  where
    addModuleBody' :: [Definition] -> Environment -> Try Environment
    addModuleBody' [] env = return env
    addModuleBody' (Definition k e : ds) env = do
      v <- valueOf e env
      addModuleBody' ds $ extendNamed name k (exprToDeno v) env

findModuleIface :: String -> Interface -> Environment
                -> Try [(String, DenotedValue)]
findModuleIface name (Interface decls) env = findIfaces decls []
  where
    findIfaces :: [Declaration] -> [(String, DenotedValue)]
               -> Try [(String, DenotedValue)]
    findIfaces [] acc = return $ reverse acc
    findIfaces (Declaration k _ : ds) acc =
      findIfaces ds ((k, forceEnvTry $ applyNamed env name k) : acc)

evalProgram :: Program -> EvaluateResult
evalProgram (Prog mDefs expr) = eval mDefs expr

valueOf :: Expression -> Environment -> EvaluateResult
valueOf (ConstExpr x) _                = evalConstExpr x
valueOf (VarExpr var) env              = evalVarExpr var env
valueOf (LetRecExpr procs recBody) env = evalLetRecExpr procs recBody env
valueOf (BinOpExpr op expr1 expr2) env = evalBinOpExpr op expr1 expr2 env
valueOf (UnaryOpExpr op expr) env      = evalUnaryOpExpr op expr env
valueOf (CondExpr pairs) env           = evalCondExpr pairs env
valueOf (LetExpr bindings body) env    = evalLetExpr bindings body env
valueOf (ProcExpr params body) env     = evalProcExpr params body env
valueOf (CallExpr rator rand) env      = evalCallExpr rator rand env
valueOf (QualifiedVarExpr m v) env     = evalQualifiedVarExpr m v env

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
evalVarExpr var env = return . denoToExpr $ applyForce env var

evalLetRecExpr :: [(Type, String, [(String, Type)], Expression)] -> Expression
               -> Environment
               -> EvaluateResult
evalLetRecExpr procsSubUnits recBody env =
  valueOf recBody $ extendRecMany noTypedProcs env
  where
    func (_, name, params, body) = (name, fmap fst params, body)
    noTypedProcs = fmap func procsSubUnits

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

findOp :: (Eq a, Show a) => a -> [(a, b)] -> b
findOp op lst = fromMaybe err $ lookup op lst
  where err = error $ "Unknown operator: " `mappend` show op

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
  let func = findOp op binOps
  v1 <- valueOf expr1 env
  v2 <- valueOf expr2 env
  func v1 v2

evalUnaryOpExpr :: UnaryOp -> Expression -> Environment
                -> EvaluateResult
evalUnaryOpExpr op expr env = do
  let func = findOp op unaryOps
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

evalProcExpr :: [(String, Type)] -> Expression -> Environment -> EvaluateResult
evalProcExpr params body env =
  return . ExprProc $ Procedure untypedParams body env
  where untypedParams = fmap fst params

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

evalQualifiedVarExpr :: String -> String -> Environment -> EvaluateResult
evalQualifiedVarExpr mName vName env =
  return . denoToExpr . forceEnvTry $ applyNamed env mName vName
