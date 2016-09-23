module ImplicitRefs.Evaluator
( valueOf
, run
, eval
, evalProgram
) where

import           Control.Applicative  ((<|>))
import           Control.Arrow        (second)
import           Control.Monad.Except
import           ImplicitRefs.Data
import           ImplicitRefs.Parser

type EvaluateResult = IOTry ExpressedValue

liftMaybe :: LangError -> Maybe a -> IOTry a
liftMaybe _ (Just x) = return x
liftMaybe y Nothing  = throwError y

run :: String -> IO (Try ExpressedValue)
run input = runExceptT $ do
  prog <- liftTry (parseProgram input)
  store <- liftIO initStore
  evalProgram store prog

evalProgram :: Store -> Program -> EvaluateResult
evalProgram store (Prog expr) = eval store expr

eval :: Store -> Expression -> EvaluateResult
eval store expr = valueOf expr empty store

valueOf :: Expression -> Environment -> Store -> EvaluateResult
valueOf (ConstExpr x) _ _                = evalConstExpr x
valueOf (VarExpr var) env s              = evalVarExpr var env s
valueOf (LetRecExpr procs recBody) env s = evalLetRecExpr procs recBody env s
valueOf (BinOpExpr op expr1 expr2) env s = evalBinOpExpr op expr1 expr2 env s
valueOf (UnaryOpExpr op expr) env s      = evalUnaryOpExpr op expr env s
valueOf (CondExpr pairs) env s           = evalCondExpr pairs env s
valueOf (LetExpr bindings body) env s    = evalLetExpr bindings body env s
valueOf (ProcExpr params body) env _     = evalProcExpr params body env
valueOf (CallExpr rator rands) env s     = evalCallExpr rator rands env s
valueOf (BeginExpr exprs) env s          = evalBeginExpr exprs env s
valueOf (AssignExpr name expr) env s     = evalAssignExpr name expr env s
valueOf (SetDynamicExpr n e b) env s     = evalSetDynamicExpr n e b env s
valueOf (RefExpr name) env s             = evalRefExpr name env
valueOf (DeRefExpr name) env s           = evalDeRefExpr name env s
valueOf (SetRefExpr name expr) env s     = evalSetRefExpr name expr env s

evalRefExpr :: String -> Environment -> EvaluateResult
evalRefExpr name env = do
  ref <- getRef env name
  return $ ExprRef ref

unpackExprRef :: ExpressedValue -> IOTry Ref
unpackExprRef (ExprRef ref) = return ref
unpackExprRef notRef        = throwError $ TypeMismatch "reference" notRef

unpackProc :: ExpressedValue -> IOTry Procedure
unpackProc (ExprProc proc) = return proc
unpackProc notProc         = throwError $ TypeMismatch "procedure" notProc

getExprRef :: String -> Environment -> Store -> IOTry Ref
getExprRef name env store = do
  refRef <- getRef env name
  refVal <- deRef store refRef
  unpackExprRef refVal

evalDeRefExpr :: String -> Environment -> Store -> EvaluateResult
evalDeRefExpr name env store = do
  ref <- getExprRef name env store
  deRef store ref

evalSetRefExpr :: String -> Expression -> Environment -> Store -> EvaluateResult
evalSetRefExpr name expr env store = do
  ref <- getExprRef name env store
  val <- valueOf expr env store
  setRef store ref val
  return $ ExprBool False

evalSetDynamicExpr :: String -> Expression -> Expression -> Environment -> Store
                   -> EvaluateResult
evalSetDynamicExpr name expr body env store = do
  ref <- getRef env name
  oldVal <- deRef store ref
  newVal <- valueOf expr env store
  setRef store ref newVal
  result <- valueOf body env store
  setRef store ref oldVal
  return result

getRef :: Environment -> String -> IOTry Ref
getRef env name = case apply env name of
  Just (DenoRef ref) -> return ref
  Nothing            -> throwError $ UnboundVar name

evalAssignExpr :: String -> Expression -> Environment -> Store -> EvaluateResult
evalAssignExpr name expr env store = do
  val <- valueOf expr env store
  ref <- getRef env name
  setRef store ref val
  return $ ExprBool False

evalBeginExpr :: [Expression] -> Environment -> Store -> EvaluateResult
evalBeginExpr exprs env store = foldl func (return $ ExprBool False) exprs
  where
    func acc ele = do
      acc
      valueOf ele env store

evalExpressionList :: [Expression] -> Environment -> Store
                   -> IOTry [ExpressedValue]
evalExpressionList lst env store = reverse <$> evaledList
  where
    func acc expr = do
      lst <- acc
      ele <- valueOf expr env store
      return $ ele:lst
    evaledList = foldl func (return []) lst

evalConstExpr :: ExpressedValue -> EvaluateResult
evalConstExpr = return

evalVarExpr :: String -> Environment -> Store -> EvaluateResult
evalVarExpr name env store = do
  denoRef <- liftMaybe (UnboundVar name) (apply env name)
  let (DenoRef ref) = denoRef
  deRef store ref

evalLetRecExpr :: [(String, [String], Expression)] -> Expression
               -> Environment -> Store
               -> EvaluateResult
evalLetRecExpr procsSubUnits recBody env store = do
  newEnv <- extendRecMany store procsSubUnits env
  valueOf recBody newEnv store

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

unpackNum :: ExpressedValue -> IOTry Integer
unpackNum (ExprNum n) = return n
unpackNum notNum      = throwError $ TypeMismatch "number" notNum

unpackBool :: ExpressedValue -> IOTry Bool
unpackBool (ExprBool b) = return b
unpackBool notBool      = throwError $ TypeMismatch "boolean" notBool

tryFind :: Eq a => LangError -> a -> [(a, b)] -> IOTry b
tryFind err x pairs = liftMaybe err (lookup x pairs)

tryFindOp :: (Eq a, Show a) => a -> [(a, b)] -> IOTry b
tryFindOp op = tryFind (UnknownOperator $ show op) op

binOpConverter :: (ExpressedValue -> IOTry a)
               -> (ExpressedValue -> IOTry b)
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

unaryOpConverter :: (ExpressedValue -> IOTry a)
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

evalBinOpExpr :: BinOp -> Expression -> Expression -> Environment -> Store
              -> EvaluateResult
evalBinOpExpr op expr1 expr2 env store = do
  func <- tryFindOp op binOps
  v1 <- valueOf expr1 env store
  v2 <- valueOf expr2 env store
  func v1 v2

evalUnaryOpExpr :: UnaryOp -> Expression -> Environment -> Store
                -> EvaluateResult
evalUnaryOpExpr op expr env store = do
  func <- tryFindOp op unaryOps
  v <- valueOf expr env store
  func v

evalCondExpr :: [(Expression, Expression)] -> Environment -> Store
             -> EvaluateResult
evalCondExpr [] _ _ = throwError $ RuntimeError "No predicate is true."
evalCondExpr ((e1, e2):pairs) env store = do
  val <- valueOf e1 env store
  bool <- unpackBool val
  if bool then valueOf e2 env store else evalCondExpr pairs env store

evalLetExpr :: [(String, Expression)] -> Expression -> Environment -> Store
            -> EvaluateResult
evalLetExpr bindings body env store = evalLetExpr' bindings body env
  where
    evalLetExpr' [] body newEnv = valueOf body newEnv store
    evalLetExpr' ((name, expr):xs) body newEnv = do
      val <- valueOf expr env store
      ref <- newRef store val
      evalLetExpr' xs body (extend name (DenoRef ref) newEnv)

evalProcExpr :: [String] -> Expression -> Environment -> EvaluateResult
evalProcExpr params body env = return . ExprProc $ Procedure params body env

evalCallExpr :: Expression -> [Expression] -> Environment -> Store
             -> EvaluateResult
evalCallExpr ratorExpr randExprs env store = do
  rator <- valueOf ratorExpr env store
  content <- unpackProc rator
  rands <- evalExpressionList randExprs env store
  applyProcedure content rands
  where
    safeZip :: [String] -> [ExpressedValue] -> IOTry [(String, ExpressedValue)]
    safeZip as bs =
      let na = length as
          nb = length bs
      in if na /= nb
           then throwError $ ArgNumMismatch (toInteger na) bs
           else return $ zip as bs
    allocateAll :: [(String, ExpressedValue)] -> Environment
                -> IOTry Environment
    allocateAll [] env = return env
    allocateAll ((name, val):pairs) env = do
      ref <- newRef store val
      allocateAll pairs (extend name (DenoRef ref) env)
    applyProcedure :: Procedure -> [ExpressedValue] -> EvaluateResult
    applyProcedure (Procedure params body savedEnv) rands = do
      pairs <- safeZip params rands
      newEnv <- allocateAll pairs savedEnv
      valueOf body newEnv store
