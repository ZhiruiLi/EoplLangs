module CallByName.Evaluator
( valueOf
, run
, eval
, evalProgram
) where

import           CallByName.Data
import           CallByName.Parser
import           Control.Monad.Trans.State.Lazy (evalStateT)

type EvaluateResult = StatedTry ExpressedValue

liftMaybe :: String -> Maybe a -> StatedTry a
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
valueOf (CallExpr rator rands) env     = evalCallExpr rator rands env
valueOf (BeginExpr exprs) env          = evalBeginExpr exprs env
valueOf (AssignExpr name expr) env     = evalAssignExpr name expr env

getRef :: Environment -> String -> StatedTry Ref
getRef env name = do
  deno <- getDeno env name
  let (DenoRef ref) = deno
  return ref

getDeno :: Environment -> String -> StatedTry DenotedValue
getDeno env name = liftMaybe
  ("Not in scope: " ++ show name) (apply env name)

evalAssignExpr :: String -> Expression -> Environment -> EvaluateResult
evalAssignExpr name expr env = do
  val <- valueOf expr env
  ref <- getRef env name
  _ <- setRef ref val
  return $ ExprBool False

evalBeginExpr :: [Expression] -> Environment -> EvaluateResult
evalBeginExpr [] env = throwError
  "begin expression should at least have one sub expression"
evalBeginExpr exprs env = foldl func (return $ ExprBool False) exprs
  where
    func acc ele = do
      _ <- acc
      valueOf ele env

evalExpressionList :: [Expression] -> Environment -> StatedTry [ExpressedValue]
evalExpressionList lst env = reverse <$> evaledList
  where
    func acc expr = do
      lst <- acc
      ele <- valueOf expr env
      return $ ele:lst
    evaledList = foldl func (return []) lst

evalConstExpr :: ExpressedValue -> EvaluateResult
evalConstExpr = return

evalVarExpr :: String -> Environment -> EvaluateResult
evalVarExpr name env = do
  ref <- getRef env name
  maybeVal <- deRef ref
  case maybeVal of
    ExprThunk (Thunk expr savedEnv) -> valueOf expr savedEnv
    _                               -> return maybeVal

evalLetRecExpr :: [(String, [String], Expression)] -> Expression -> Environment
               -> EvaluateResult
evalLetRecExpr procsSubUnits recBody env = do
  newEnv <- extendRecMany procsSubUnits env
  valueOf recBody newEnv

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
  case ( lookup op unaryNumToNumOpMap
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
evalLetExpr bindings body env = evalLetExpr' bindings body env
  where
    evalLetExpr' [] body newEnv = valueOf body newEnv
    evalLetExpr' ((name, expr):xs) body newEnv = do
      val <- valueOf expr env
      ref <- newRef val
      evalLetExpr xs body (extend name (DenoRef ref) newEnv)

evalProcExpr :: [String] -> Expression -> Environment -> EvaluateResult
evalProcExpr params body env = return . ExprProc $ Procedure params body env

evalCallExpr :: Expression -> [Expression] -> Environment -> EvaluateResult
evalCallExpr ratorExpr randExprs env = do
  rator <- valueOf ratorExpr env
  proc <- checkProc rator
  refs <- valueOfOperands randExprs env
  applyProcedure proc refs
  where
    -- | Check if the operand expression is variable expression, if it is,
    -- refer to the same location, otherwise, build a new thunk and create
    -- a new reference to this thunk.
    valueOfOperands :: [Expression] -> Environment -> StatedTry [Ref]
    valueOfOperands [] _ = return []
    valueOfOperands (VarExpr name : exprs) env =
      recOpVal exprs env (getRef env name)
    valueOfOperands (ConstExpr val : exprs) env =
      recOpVal exprs env (newRef val)
    valueOfOperands (ProcExpr params body : exprs) env =
      recOpVal exprs env (newRef . ExprProc $ Procedure params body env)
    valueOfOperands (expr:exprs) env =
      recOpVal exprs env (newRef (ExprThunk (Thunk expr env)))
    recOpVal :: [Expression] -> Environment -> StatedTry Ref -> StatedTry [Ref]
    recOpVal exprs env tryRef = (:) <$> tryRef <*> valueOfOperands exprs env
    checkProc :: ExpressedValue -> StatedTry Procedure
    checkProc (ExprProc proc) = return proc
    checkProc noProc = throwError $
      "Operator of call expression should be procedure, but got: "
      `mappend` show noProc
    safeZip :: [a] -> [b] -> StatedTry [(a, b)]
    safeZip [] []         = return []
    safeZip (_:_) []      = throwError "Not enough arguments!"
    safeZip [] (_:_)      = throwError "Too many arguments!"
    safeZip (x:xs) (y:ys) = ((x, y):) <$> safeZip xs ys
    applyProcedure :: Procedure -> [Ref] -> EvaluateResult
    applyProcedure (Procedure params body savedEnv) rands = do
      pairs <- safeZip params (fmap DenoRef rands)
      valueOf body (extendMany pairs savedEnv)
