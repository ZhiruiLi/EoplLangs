module ImplicitRefsCont.Evaluator
( valueOf
, run
, eval
, evalProgram
) where

import           Control.Applicative     ((<|>))
import           Control.Arrow           (second)
import           Control.Monad.Except
import           ImplicitRefsCont.Data
import           ImplicitRefsCont.Parser

type EvaluateResult = IOTry ExpressedValue

applyCont :: Store -> Continuation -> ExpressedValue -> EvaluateResult
applyCont _ EndCont val = return val
applyCont store (UnaryOpCont func cont) val =
  func val >>= applyCont store cont
applyCont store (BinOpCont1 func expr2 env cont) val1 =
  valueOf expr2 env store (BinOpCont2 func val1 cont)
applyCont store (BinOpCont2 func val1 cont) val2 =
  func val1 val2 >>= applyCont store cont
applyCont store (IfCont thenE elseE env cont) val = do
  bool <- unpackBool val
  let body = if bool then thenE else elseE
  valueOf body env store cont
applyCont store (LetCont name (pair : pairs) acc body env cont) val = do
  ref <- newRef store val
  let (nextName, nextExpr) = pair
  let curr = (name, DenoRef ref)
  let nextCont = LetCont nextName pairs (curr : acc) body env cont
  valueOf nextExpr env store nextCont
applyCont store (LetCont name [] acc body env cont) val = do
  ref <- newRef store val
  let bindings = reverse $ (name, DenoRef ref) : acc
  valueOf body (extendMany bindings env) store cont
applyCont store (RatorCont [] env cont) val = do
  proc <- unpackProc val
  applyProcedure store proc [] cont
applyCont store (RatorCont (rand : rands) env cont) val =
  valueOf rand env store (RandCont rands val [] env cont)
applyCont store (RandCont (rand : rands) rator valsAcc env cont) val =
  valueOf rand env store (RandCont rands rator (val : valsAcc) env cont)
applyCont store (RandCont [] rator valsAcc env cont) val = do
  let rands = reverse (val : valsAcc)
  proc <- unpackProc rator
  applyProcedure store proc rands cont
applyCont store (AssignCont name env cont) val = do
  ref <- getRef env name
  setRef store ref val
  applyCont store cont (ExprBool False)
applyCont store (BeginCont [] env cont) val =
  applyCont store cont val
applyCont store (BeginCont (expr : exprs) env cont) _ =
  valueOf expr env store (BeginCont exprs env cont)

liftMaybe :: LangError -> Maybe a -> IOTry a
liftMaybe _ (Just x) = return x
liftMaybe y Nothing  = throwError y

run :: String -> IO (Try ExpressedValue)
run input = runExceptT $ do
  prog <- liftTry (parseProgram input)
  store <- liftIO initStore
  evalProgram store prog

evalProgram :: Store -> Program -> EvaluateResult
evalProgram store (Prog expr) = eval expr store

eval :: Expression -> Store -> EvaluateResult
eval expr store = valueOf expr empty store EndCont

valueOf :: Expression -> Environment -> Store -> Continuation -> EvaluateResult
valueOf (ConstExpr x) _ s c             = evalConstExpr x s c
valueOf (VarExpr var) env s c           = evalVarExpr var env s c
valueOf (LetRecExpr procs body) env s c = evalLetRecExpr procs body env s c
valueOf (BinOpExpr op e1 e2) env s c    = evalBinOpExpr op e1 e2 env s c
valueOf (UnaryOpExpr op expr) env s c   = evalUnaryOpExpr op expr env s c
valueOf (IfExpr e1 e2 e3) env s c       = evalIfExpr e1 e2 e3 env s c
valueOf (LetExpr bindings body) env s c = evalLetExpr bindings body env s c
valueOf (ProcExpr params body) env s c  = evalProcExpr params body env s c
valueOf (CallExpr rator rands) env s c  = evalCallExpr rator rands env s c
valueOf (BeginExpr exprs) env s c       = evalBeginExpr exprs env s c
valueOf (AssignExpr name expr) env s c  = evalAssignExpr name expr env s c

unpackExprRef :: ExpressedValue -> IOTry Ref
unpackExprRef (ExprRef ref) = return ref
unpackExprRef notRef        = throwError $ TypeMismatch "reference" notRef

unpackProc :: ExpressedValue -> IOTry Procedure
unpackProc (ExprProc proc) = return proc
unpackProc noProc          = throwError $ TypeMismatch "procedure" noProc

evalConstExpr :: ExpressedValue -> Store -> Continuation -> EvaluateResult
evalConstExpr val store cont = applyCont store cont val

evalVarExpr :: String -> Environment -> Store -> Continuation -> EvaluateResult
evalVarExpr name env store cont = do
  denoRef <- liftMaybe (UnboundVar name) (apply env name)
  let (DenoRef ref) = denoRef
  deRef store ref >>= applyCont store cont

evalLetRecExpr :: [(String, [String], Expression)] -> Expression
               -> Environment -> Store -> Continuation
               -> EvaluateResult
evalLetRecExpr procsSubUnits recBody env store cont = do
  newEnv <- extendRecMany store procsSubUnits env
  valueOf recBody newEnv store cont

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
              -> Environment -> Store -> Continuation
              -> EvaluateResult
evalBinOpExpr op expr1 expr2 env store cont = do
  func <- tryFindOp op binOps
  valueOf expr1 env store (BinOpCont1 func expr2 env cont)

evalUnaryOpExpr :: UnaryOp -> Expression
                -> Environment -> Store -> Continuation
                -> EvaluateResult
evalUnaryOpExpr op expr env store cont = do
  func <- tryFindOp op unaryOps
  valueOf expr env store (UnaryOpCont func cont)

evalIfExpr :: Expression -> Expression -> Expression
           -> Environment -> Store -> Continuation
           -> EvaluateResult
evalIfExpr ifE thenE elseE env store cont =
  valueOf ifE env store (IfCont thenE elseE env cont)

evalLetExpr :: [(String, Expression)] -> Expression
            -> Environment -> Store -> Continuation
            -> EvaluateResult
evalLetExpr [] body env store cont = valueOf body env store cont
evalLetExpr ((name, expr) : pairs) body env store cont =
  valueOf expr env store (LetCont name pairs [] body env cont)

evalProcExpr :: [String] -> Expression -> Environment -> Store -> Continuation
             -> EvaluateResult
evalProcExpr params body env store cont =
  applyCont store cont (ExprProc $ Procedure params body env)

evalCallExpr :: Expression -> [Expression]
             -> Environment -> Store -> Continuation
             -> EvaluateResult
evalCallExpr ratorExpr randExprs env store cont =
  valueOf ratorExpr env store (RatorCont randExprs env cont)

applyProcedure :: Store -> Procedure -> [ExpressedValue] -> Continuation
               -> EvaluateResult
applyProcedure store (Procedure params body savedEnv) rands cont = do
  pairs <- safeZip params rands
  newEnv <- allocateAll pairs savedEnv
  valueOf body newEnv store cont
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

evalAssignExpr :: String -> Expression -> Environment -> Store -> Continuation
               -> EvaluateResult
evalAssignExpr name expr env store cont =
  valueOf expr env store (AssignCont name env cont)

evalBeginExpr :: [Expression] -> Environment -> Store -> Continuation
              -> EvaluateResult
evalBeginExpr [] _ _ _ = error
  "Begin expression should contain a least one epxression."
evalBeginExpr (expr : exprs) env store cont =
  valueOf expr env store (BeginCont exprs env cont)
