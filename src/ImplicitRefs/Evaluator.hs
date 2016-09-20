module ImplicitRefs.Evaluator
( valueOf
, run
, eval
, evalProgram
) where

import           Control.Applicative            ((<|>))
import           Control.Arrow                  (second)
import           Control.Monad.Trans.State.Lazy (evalStateT)
import           ImplicitRefs.Data
import           ImplicitRefs.Parser

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
valueOf (SetDynamicExpr n e b) env     = evalSetDynamicExpr n e b env
valueOf (RefExpr name) env             = evalRefExpr name env
valueOf (DeRefExpr name) env           = evalDeRefExpr name env
valueOf (SetRefExpr name expr) env     = evalSetRefExpr name expr env

evalRefExpr :: String -> Environment -> EvaluateResult
evalRefExpr name env = do
  ref <- getRef env name
  return $ ExprRef ref

unpackExprRef :: ExpressedValue -> String -> StatedTry Ref
unpackExprRef (ExprRef ref) _ = return ref
unpackExprRef notRef name = throwError $ concat
  [ "Operand of ", name, " should be reference value, but got: ", show notRef ]

evalDeRefExpr :: String -> Environment -> EvaluateResult
evalDeRefExpr name env = do
  refRef <- getRef env name
  refVal <- deRef refRef
  ref <- unpackExprRef refVal "deref"
  deRef ref

evalSetRefExpr :: String -> Expression -> Environment -> EvaluateResult
evalSetRefExpr name expr env = do
  refRef <- getRef env name
  refVal <- deRef refRef
  ref <- unpackExprRef refVal "setref"
  val <- valueOf expr env
  _ <- setRef ref val
  return $ ExprBool False

evalSetDynamicExpr :: String -> Expression -> Expression -> Environment
                   -> EvaluateResult
evalSetDynamicExpr name expr body env = do
  ref <- getRef env name
  oldVal <- deRef ref
  newVal <- valueOf expr env
  _ <- setRef ref newVal
  result <- valueOf body env
  _ <- setRef ref oldVal
  return result

getRef :: Environment -> String -> StatedTry Ref
getRef env name = case apply env name of
  Just (DenoRef ref) -> return ref
  Nothing            -> throwError $ "Not in scope: " `mappend` show name

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
  denoRef <- liftMaybe ("Not in scope: " `mappend` name)
                   (apply env name)
  let (DenoRef ref) = denoRef
  deRef ref

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

unpackNum :: String -> ExpressedValue -> StatedTry Integer
unpackNum _ (ExprNum n) = return n
unpackNum caller notNum = throwError $ concat [
  caller, ": Unpacking a not number value: ", show notNum ]

unpackBool :: String -> ExpressedValue -> StatedTry Bool
unpackBool _ (ExprBool b) = return b
unpackBool caller notBool = throwError $ concat [
  caller, ": Unpacking a not boolean value: ", show notBool ]

tryFind :: Eq a => String -> a -> [(a, b)] -> StatedTry b
tryFind err x pairs = liftMaybe err (lookup x pairs)

tryFindOp :: (Eq a, Show a) => a -> [(a, b)] -> StatedTry b
tryFindOp op = tryFind ("Unknown operator: " `mappend` show op) op

binOpConverter :: (String -> ExpressedValue -> StatedTry a)
               -> (String -> ExpressedValue -> StatedTry b)
               -> (c -> ExpressedValue)
               -> (a -> b -> c)
               -> (ExpressedValue -> ExpressedValue -> EvaluateResult)
binOpConverter unpack1 unpack2 trans func val1 val2 = do
  va <- unpack1 "Binary operator" val1
  vb <- unpack2 "Binary operator" val2
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

unaryOpConverter :: (String -> ExpressedValue -> StatedTry a)
                 -> (b -> ExpressedValue)
                 -> (a -> b)
                 -> (ExpressedValue -> EvaluateResult)
unaryOpConverter unpack trans func val = do
  va <- unpack "Unary operator" val
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
  content <- unpackProc rator
  rands <- evalExpressionList randExprs env
  applyProcedure content rands
  where
    unpackProc :: ExpressedValue -> StatedTry Procedure
    unpackProc (ExprProc proc) = return proc
    unpackProc noProc = throwError $
      "Operator of call expression should be procedure, but got: "
      `mappend` show noProc
    safeZip :: [a] -> [b] -> StatedTry [(a, b)]
    safeZip [] []         = return []
    safeZip (_:_) []      = throwError "Not enough arguments!"
    safeZip [] (_:_)      = throwError "Too many arguments!"
    safeZip (x:xs) (y:ys) = ((x, y):) <$> safeZip xs ys
    allocateAll :: [(String, ExpressedValue)] -> Environment
                -> StatedTry Environment
    allocateAll [] env = return env
    allocateAll ((name, val):pairs) env = do
      ref <- newRef val
      allocateAll pairs (extend name (DenoRef ref) env)
    applyProcedure :: Procedure -> [ExpressedValue] -> EvaluateResult
    applyProcedure (Procedure params body savedEnv) rands = do
      pairs <- safeZip params rands
      newEnv <- allocateAll pairs savedEnv
      valueOf body newEnv
