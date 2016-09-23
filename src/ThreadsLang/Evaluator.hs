module ThreadsLang.Evaluator
( valueOf
, run
, eval
, evalProgram
) where

import           Control.Applicative  ((<|>))
import           Control.Arrow        (second)
import           Control.Monad.Except
import           Debug.Trace          (trace)
import           ThreadsLang.Data
import           ThreadsLang.Parser

type EvaluateResult = IOTry ExpressedValue

run :: String -> IO (Try ExpressedValue)
run input = runExceptT $ do
  prog <- liftTry (parseProgram input)
  store <- liftIO initStore
  evalProgram store 3 prog

evalProgram :: Store -> Integer -> Program -> EvaluateResult
evalProgram store timeSlice (Prog expr) = do
  scheduler <- liftIO $ initScheduler timeSlice
  eval store scheduler expr

eval :: Store -> Scheduler -> Expression -> EvaluateResult
eval store scheduler expr = valueOf expr empty store scheduler EndCont

applyCont :: Store -> Scheduler -> Continuation -> ExpressedValue
          -> EvaluateResult
applyCont store sch continuation val = do
  isExpired <- liftIO $ timeExpired sch
  if isExpired
    then do { liftIO . enqueueThread sch $
                Thread (applyCont store sch continuation val)
            ; runNextThread sch
            }
    else do { liftIO $ decrementTime sch
            ; applyCont' continuation
            }
  where
    applyCont' EndCont = do
      liftIO $ setFinalResult sch val
      runNextThread sch
    applyCont' EndSubThreadCont = runNextThread sch
    applyCont' (UnaryOpCont func cont) =
      func val >>= applyCont store sch cont
    applyCont' (BinOpCont1 func expr2 env cont) =
      valueOf expr2 env store sch (BinOpCont2 func val cont)
    applyCont' (BinOpCont2 func val1 cont) =
      func val1 val >>= applyCont store sch cont
    applyCont' (IfCont thenE elseE env cont) = do
      bool <- unpackBool val
      let body = if bool then thenE else elseE
      valueOf body env store sch cont
    applyCont' (LetCont name (pair : pairs) acc body env cont) = do
      ref <- newRef store val
      let (nextName, nextExpr) = pair
      let curr = (name, DenoRef ref)
      let nextCont = LetCont nextName pairs (curr : acc) body env cont
      valueOf nextExpr env store sch nextCont
    applyCont' (LetCont name [] acc body env cont) = do
      ref <- newRef store val
      let bindings = reverse $ (name, DenoRef ref) : acc
      valueOf body (extendMany bindings env) store sch cont
    applyCont' (RatorCont [] env cont) = do
      proc <- unpackProc val
      applyProcedure store sch proc [] cont
    applyCont' (RatorCont (rand : rands) env cont) =
      valueOf rand env store sch (RandCont rands val [] env cont)
    applyCont' (RandCont (rand : rands) rator valsAcc env cont) =
      valueOf rand env store sch
              (RandCont rands rator (val : valsAcc) env cont)
    applyCont' (RandCont [] rator valsAcc env cont) = do
      let rands = reverse (val : valsAcc)
      proc <- unpackProc rator
      applyProcedure store sch proc rands cont
    applyCont' (AssignCont name env cont) = do
      ref <- getRef env name
      setRef store ref val
      applyCont store sch cont (ExprBool False)
    applyCont' (BeginCont [] env cont) =
      applyCont store sch cont val
    applyCont' (BeginCont (expr : exprs) env cont) =
      valueOf expr env store sch (BeginCont exprs env cont)
    applyCont' (SpawnCont cont) = do
      proc <- unpackProc val
      let thread = Thread (applyProcedure store sch proc [] EndSubThreadCont)
      liftIO $ enqueueThread sch thread
      applyCont store sch cont (ExprBool False)

liftMaybe :: LangError -> Maybe a -> IOTry a
liftMaybe _ (Just x) = return x
liftMaybe y Nothing  = throwError y

valueOf :: Expression -> Environment -> Store -> Scheduler -> Continuation
        -> EvaluateResult
valueOf (ConstExpr x) _ s sch c = evalConstExpr x s sch c
valueOf (VarExpr var) env s sch c = evalVarExpr var env s sch c
valueOf (LetRecExpr ps body) env s sch c = evalLetRecExpr ps body env s sch c
valueOf (BinOpExpr op e1 e2) env s sch c = evalBinOpExpr op e1 e2 env s sch c
valueOf (UnaryOpExpr op expr) env s sch c = evalUnaryOpExpr op expr env s sch c
valueOf (IfExpr e1 e2 e3) env s sch c = evalIfExpr e1 e2 e3 env s sch c
valueOf (LetExpr binds body) env s sch c = evalLetExpr binds body env s sch c
valueOf (ProcExpr params body) env s sch c = evalProcExpr params body env s sch c
valueOf (CallExpr rator rands) env s sch c = evalCallExpr rator rands env s sch c
valueOf (BeginExpr es) env s sch c = evalBeginExpr es env s sch c
valueOf (AssignExpr n e) env s sch c = evalAssignExpr n e env s sch c
valueOf (SpawnExpr expr) env s sch c = evalSpawnExpr expr env s sch c

unpackExprRef :: ExpressedValue -> IOTry Ref
unpackExprRef (ExprRef ref) = return ref
unpackExprRef notRef        = throwError $ TypeMismatch "reference" notRef

unpackProc :: ExpressedValue -> IOTry Procedure
unpackProc (ExprProc proc) = return proc
unpackProc noProc          = throwError $ TypeMismatch "procedure" noProc

evalConstExpr :: ExpressedValue -> Store -> Scheduler -> Continuation
              -> EvaluateResult
evalConstExpr val store scheduler cont = applyCont store scheduler cont val

evalVarExpr :: String -> Environment -> Store -> Scheduler -> Continuation
            -> EvaluateResult
evalVarExpr name env store scheduler cont = do
  denoRef <- liftMaybe (UnboundVar name) (apply env name)
  let (DenoRef ref) = denoRef
  deRef store ref >>= applyCont store scheduler cont

evalLetRecExpr :: [(String, [String], Expression)] -> Expression
               -> Environment -> Store -> Scheduler -> Continuation
               -> EvaluateResult
evalLetRecExpr procsSubUnits recBody env store scheduler cont = do
  newEnv <- extendRecMany store procsSubUnits env
  valueOf recBody newEnv store scheduler cont

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

evalBinOpExpr :: BinOp -> Expression -> Expression
              -> Environment -> Store -> Scheduler -> Continuation
              -> EvaluateResult
evalBinOpExpr op expr1 expr2 env store scheduler cont = do
  func <- tryFindOp op binOps
  valueOf expr1 env store scheduler (BinOpCont1 func expr2 env cont)

evalUnaryOpExpr :: UnaryOp -> Expression
                -> Environment -> Store -> Scheduler -> Continuation
                -> EvaluateResult
evalUnaryOpExpr op expr env store scheduler cont = do
  func <- tryFindOp op unaryOps
  valueOf expr env store scheduler (UnaryOpCont func cont)

evalIfExpr :: Expression -> Expression -> Expression
           -> Environment -> Store -> Scheduler -> Continuation
           -> EvaluateResult
evalIfExpr ifE thenE elseE env store scheduler cont =
  valueOf ifE env store scheduler (IfCont thenE elseE env cont)

evalLetExpr :: [(String, Expression)] -> Expression
            -> Environment -> Store -> Scheduler -> Continuation
            -> EvaluateResult
evalLetExpr [] body env store scheduler cont =
  valueOf body env store scheduler cont
evalLetExpr ((name, expr) : pairs) body env store scheduler cont =
  valueOf expr env store scheduler (LetCont name pairs [] body env cont)

evalProcExpr :: [String] -> Expression
             -> Environment -> Store -> Scheduler -> Continuation
             -> EvaluateResult
evalProcExpr params body env store scheduler cont =
  applyCont store scheduler cont (ExprProc $ Procedure params body env)

evalCallExpr :: Expression -> [Expression]
             -> Environment -> Store -> Scheduler -> Continuation
             -> EvaluateResult
evalCallExpr ratorExpr randExprs env store scheduler cont =
  valueOf ratorExpr env store scheduler (RatorCont randExprs env cont)

applyProcedure :: Store -> Scheduler
               -> Procedure -> [ExpressedValue] -> Continuation
               -> EvaluateResult
applyProcedure store scheduler (Procedure params body savedEnv) rands cont = do
  pairs <- safeZip params rands
  newEnv <- allocateAll pairs savedEnv
  valueOf body newEnv store scheduler cont
  where
    allocateAll :: [(String, ExpressedValue)] -> Environment
                -> IOTry Environment
    allocateAll [] env = return env
    allocateAll ((name, val) : pairs) env = do
      ref <- newRef store val
      allocateAll pairs (extend name (DenoRef ref) env)

safeZip :: [String] -> [ExpressedValue] -> IOTry [(String, ExpressedValue)]
safeZip as bs =
  let na = length as
      nb = length bs
  in if na /= nb
       then throwError $ ArgNumMismatch (toInteger na) bs
       else return $ zip as bs

getExprRef :: String -> Environment -> Store -> IOTry Ref
getExprRef name env store = do
  refRef <- getRef env name
  refVal <- deRef store refRef
  unpackExprRef refVal

getRef :: Environment -> String -> IOTry Ref
getRef env name = case apply env name of
  Just (DenoRef ref) -> return ref
  Nothing            -> throwError $ UnboundVar name

evalAssignExpr :: String -> Expression
               -> Environment -> Store -> Scheduler -> Continuation
               -> EvaluateResult
evalAssignExpr name expr env store scheduler cont =
  valueOf expr env store scheduler (AssignCont name env cont)

evalBeginExpr :: [Expression]
              -> Environment -> Store -> Scheduler -> Continuation
              -> EvaluateResult
evalBeginExpr [] _ _ _ _ = error
  "Begin expression should contain a least one epxression."
evalBeginExpr (expr : exprs) env store scheduler cont =
  valueOf expr env store scheduler (BeginCont exprs env cont)

evalSpawnExpr :: Expression
              -> Environment -> Store -> Scheduler -> Continuation
              -> EvaluateResult
evalSpawnExpr expr env store scheduler cont =
  valueOf expr env store scheduler (SpawnCont cont)
