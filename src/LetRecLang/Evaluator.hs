module LetRecLang.Evaluator
( valueOf
, run
, eval
, evalProgram
) where

import           Control.Arrow     (second)
import           LetRecLang.Data
import           LetRecLang.Parser

type EvaluateResult = Try ExpressedValue

liftMaybe :: a -> Maybe b -> Either a b
liftMaybe _ (Just x) = Right x
liftMaybe y Nothing  = Left y

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
  denoToExpr <$> liftMaybe ("Not in scope: " `mappend` var) (applySafe env var)

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

evalBinOpExpr :: BinOp -> Expression -> Expression -> Environment
              -> EvaluateResult
evalBinOpExpr op expr1 expr2 env = do
  val1 <- valueOf expr1 env
  val2 <- valueOf expr2 env
  case ( lookup op binNumToNumOpMap
       , lookup op binNumToBoolOpMap
       , lookup op binBoolOpMap
       ) of
    (Just func, _, _) -> case (val1, val2) of
      (ExprNum n1, ExprNum n2) -> return . ExprNum $ func n1 n2
      (a, b)                   -> opError "number" op a b
    (_, Just func, _) -> case (val1, val2) of
      (ExprNum n1, ExprNum n2) -> return . ExprBool $ func n1 n2
      (a, b)                   -> opError "number" op a b
    (_, _, Just func) -> case (val1, val2) of
      (ExprBool b1, ExprBool b2) -> return . ExprBool $ func b1 b2
      (a, b)                     -> opError "boolean value" op a b
    _ -> invalidOpError op
  where
    opError typeName op a b = Left $ concat
      [ "Operands of binary ", show op, " operator "
      , "should both be ", typeName, "s, but got: "
      , show a, " and ", show b
      ]

invalidOpError op = error $ "Invalid operator: " `mappend` show op

evalUnaryOpExpr :: UnaryOp -> Expression -> Environment
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

evalCondExpr :: [(Expression, Expression)] -> Environment -> EvaluateResult
evalCondExpr [] _ = Left "No predicate is true"
evalCondExpr ((e1, e2):pairs) env = do
  val <- valueOf e1 env
  case val of
    ExprBool True -> valueOf e2 env
    ExprBool False -> evalCondExpr pairs env
    _ -> Left $
      "Predicate expression should be boolean, but got: "
      `mappend` show val

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
  proc <- checkProc rator
  args <- maybeArgs
  applyProcedure proc args
  where
    checkProc :: ExpressedValue -> Try Procedure
    checkProc (ExprProc proc) = Right proc
    checkProc noProc = Left $
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
      Left $ "Too many parameters: " `mappend` show params
    applyProcedure' [] _ _ args _ =
      Left $ "Too many arguments: " `mappend` show args
    applyProcedure' (p:ps) body env (a:as) usedParams =
      if p `elem` usedParams
        then Left $ "Parameter name conflict: " `mappend` p
        else applyProcedure' ps body (extend p (exprToDeno a) env)
                             as (p:usedParams)
