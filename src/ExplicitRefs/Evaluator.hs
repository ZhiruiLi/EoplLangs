module ExplicitRefs.Evaluator
( valueOf
, run
, eval
, evalProgram
) where

import           Control.Monad.Trans.State.Lazy (evalStateT)
import           ExplicitRefs.Data
import           ExplicitRefs.Parser

type EvaluateResult = StatedTry ExpressedValue

liftMaybe :: String -> Maybe ExpressedValue -> EvaluateResult
liftMaybe _ (Just x) = return x
liftMaybe y Nothing  = throwError y

run :: String -> Try ExpressedValue
run input = do
  prog <- parseProgram input
  evalStateT (evalProgram prog) initStore

evalProgram :: Program -> EvaluateResult
evalProgram (Prog expr) = eval expr

eval :: Expression -> EvaluateResult
eval = flip valueOf empty

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
valueOf (NewRefExpr expr) env          = evalNewRefExpr expr env
valueOf (DeRefExpr expr) env           = evalDeRefExpr expr env
valueOf (SetRefExpr expr1 expr2) env   = evalSetRefExpr expr1 expr2 env

evalSetRefExpr :: Expression -> Expression -> Environment -> EvaluateResult
evalSetRefExpr expr1 expr2 env = do
  refVal <- valueOf expr1 env
  ref <- checkRef refVal
  newVal <- valueOf expr2 env
  _ <- setRef ref newVal
  return newVal
  where
    checkRef (ExprRef ref) = return ref
    checkRef noRef = throwError $
      "Operator of setref should be reference value, but got: "
      `mappend` show noRef

evalNewRefExpr :: Expression -> Environment -> EvaluateResult
evalNewRefExpr expr env = do
  val <- valueOf expr env
  ref <- newRef val
  return $ ExprRef ref

evalDeRefExpr :: Expression -> Environment -> EvaluateResult
evalDeRefExpr expr env = do
  val <- valueOf expr env
  case val of
    ExprRef ref -> deRef ref
    _ -> throwError $
      "Operand of deref should be reference value, but got: "
      `mappend` show val


evalConstExpr :: ExpressedValue -> EvaluateResult
evalConstExpr = return

evalVarExpr :: String -> Environment -> EvaluateResult
evalVarExpr var env =
  liftMaybe ("Not in scope: " `mappend` var) (applySafe env var)

evalLetRecExpr :: [(String, String, Expression)] -> Expression -> Environment
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
    opError typeName op a b = throwError $ concat
      [ "Operands of binary ", show op, " operator "
      , "should both be ", typeName, "s, but got: "
      , show a, " and ", show b
      ]

invalidOpError op = error $ "Invalid operator: " `mappend` show op

evalUnaryOpExpr :: UnaryOp -> Expression -> Environment
                -> EvaluateResult
evalUnaryOpExpr op expr env = do
  val <- valueOf expr env
  case (lookup op unaryNumToNumOpMap
       , lookup op unaryNumToBoolOpMap
       , lookup op unaryBoolOpMap
       ) of
    (Just func, _, _) -> case val of
      (ExprNum n) -> return . ExprNum $ func n
      _           -> opError "number" op val
    (_, Just func, _) -> case val of
      (ExprNum n) -> return . ExprBool $ func n
      _           -> opError "number" op val
    (_, _, Just func) -> case val of
      (ExprBool b) -> return . ExprBool $ func b
      _            -> opError "boolean value" op val
    _ -> invalidOpError op
    where
      opError typeName op val = throwError $ concat
        [ "Operand of ", show op , " operator "
        , "should be ", typeName, ", but got: "
        , show val
        ]

evalCondExpr :: [(Expression, Expression)] -> Environment -> EvaluateResult
evalCondExpr [] _ = throwError "No predicate is true"
evalCondExpr ((e1, e2):pairs) env = do
  val <- valueOf e1 env
  case val of
    ExprBool True -> valueOf e2 env
    ExprBool False -> evalCondExpr pairs env
    _ -> throwError $
      "Predicate expression should be boolean, but got: "
      `mappend` show val

evalLetExpr :: [(String, Expression)] -> Expression -> Environment
            -> EvaluateResult
evalLetExpr bindExprs body env = do
  binds <- evalBindExprs [] bindExprs
  valueOf body (extendMany binds env)
  where
    evalBindExprs curr [] = (return . reverse) curr
    evalBindExprs bvs ((name, expr):bes) = do
      val <- valueOf expr env
      evalBindExprs ((name, val):bvs) bes

evalProcExpr :: String -> Expression -> Environment -> EvaluateResult
evalProcExpr param body env = return $ ExprProc param body env

evalCallExpr :: Expression -> Expression -> Environment -> EvaluateResult
evalCallExpr ratorExpr randExpr env = do
  rator <- valueOf ratorExpr env
  content <- checkProc rator
  rand <- valueOf randExpr env
  applyProcedure content rand
  where
    checkProc (ExprProc name body savedEnv) = return (name, body, savedEnv)
    checkProc noProc = throwError $
      "Operator of call expression should be procedure, but got: "
      `mappend` show noProc
    applyProcedure :: (String, Expression, Environment) -> ExpressedValue
                   -> EvaluateResult
    applyProcedure (param, body, savedEnv) rand =
      valueOf body (extend param rand savedEnv)

