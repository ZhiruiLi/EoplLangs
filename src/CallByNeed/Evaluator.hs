module CallByNeed.Evaluator
( valueOf
, run
, eval
, evalProgram
) where

import           CallByNeed.Data
import           CallByNeed.Parser
import           Control.Applicative            ((<|>))
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
  ("Not in scope: " `mappend` show name) (apply env name)

evalAssignExpr :: String -> Expression -> Environment -> EvaluateResult
evalAssignExpr name expr env = do
  val <- valueOf expr env
  ref <- getRef env name
  setRef ref val
  return $ ExprBool False

evalBeginExpr :: [Expression] -> Environment -> EvaluateResult
evalBeginExpr [] env = throwError
  "begin expression should at least have one sub expression"
evalBeginExpr exprs env = foldl func (return $ ExprBool False) exprs
  where
    func acc ele = do
      acc
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
  thunkable <- deRef ref
  case thunkable of
    ExprThunk (Thunk expr savedEnv) -> evalAndSet expr savedEnv ref
    notThunk                        -> return notThunk
    where
      evalAndSet expr savedEnv ref = do
        val <- valueOf expr savedEnv
        setRef ref val
        return val

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

evalBinOpExpr :: BinOp -> Expression -> Expression -> Environment
              -> EvaluateResult
evalBinOpExpr op expr1 expr2 env = do
  v1 <- valueOf expr1 env
  v2 <- valueOf expr2 env
  numToNum v1 v2 <|> numToBool v1 v2  <|> boolToBool v1 v2
  where
    findOpFrom = tryFindOp op
    unpackN = unpackNum $ "binary operation " `mappend` show op
    unpackB = unpackBool $ "binary operation " `mappend` show op
    numToNum :: ExpressedValue -> ExpressedValue -> EvaluateResult
    numToNum val1 val2 = do
      func <- findOpFrom binNumToNumOpMap
      n1 <- unpackN val1
      n2 <- unpackN val2
      return . ExprNum $ func n1 n2
    numToBool :: ExpressedValue -> ExpressedValue -> EvaluateResult
    numToBool val1 val2 = do
      func <- findOpFrom binNumToBoolOpMap
      n1 <- unpackN val1
      n2 <- unpackN val2
      return . ExprBool $ func n1 n2
    boolToBool :: ExpressedValue -> ExpressedValue -> EvaluateResult
    boolToBool val1 val2 = do
      func <- findOpFrom binBoolOpMap
      b1 <- unpackB val1
      b2 <- unpackB val2
      return . ExprBool $ func b1 b2

evalUnaryOpExpr :: UnaryOp -> Expression -> Environment
                -> EvaluateResult
evalUnaryOpExpr op expr env = do
  v <- valueOf expr env
  numToNum v <|> numToBool v <|> boolToBool v
  where
    findOpFrom = tryFindOp op
    unpackN = unpackNum $ "unary operation " `mappend` show op
    unpackB = unpackBool $ "unary operation " `mappend` show op
    numToNum :: ExpressedValue -> EvaluateResult
    numToNum val = do
      func <- findOpFrom unaryNumToNumOpMap
      n <- unpackN val
      return . ExprNum $ func n
    numToBool :: ExpressedValue -> EvaluateResult
    numToBool val = do
      func <- findOpFrom unaryNumToBoolOpMap
      n <- unpackN val
      return . ExprBool $ func n
    boolToBool :: ExpressedValue -> EvaluateResult
    boolToBool val = do
      func <- findOpFrom unaryBoolOpMap
      b <- unpackB val
      return . ExprBool $ func b


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
    recLetExpr :: String -> StatedTry Ref
               -> [(String, Expression)] -> Expression -> Environment
               -> EvaluateResult
    recLetExpr name tryRef xs body newEnv = do
      ref <- tryRef
      evalLetExpr' xs body (extend name (DenoRef ref) newEnv)
    evalLetExpr' :: [(String, Expression)] -> Expression -> Environment
                 -> EvaluateResult
    evalLetExpr' [] body newEnv = valueOf body newEnv
    evalLetExpr' ((name, VarExpr var):xs) body newEnv =
      recLetExpr name (getRef env var) xs body newEnv
    evalLetExpr' ((name, ConstExpr val):xs) body newEnv =
      recLetExpr name (newRef val) xs body newEnv
    evalLetExpr' ((name, ProcExpr params procBody):xs) body newEnv =
      recLetExpr name
                 (newRef (ExprProc $ Procedure params procBody env))
                 xs body newEnv
    evalLetExpr' ((name, expr):xs) body newEnv =
      recLetExpr name (newRef (ExprThunk (Thunk expr env))) xs body newEnv

evalProcExpr :: [String] -> Expression -> Environment -> EvaluateResult
evalProcExpr params body env = return . ExprProc $ Procedure params body env

evalCallExpr :: Expression -> [Expression] -> Environment -> EvaluateResult
evalCallExpr ratorExpr randExprs env = do
  rator <- valueOf ratorExpr env
  proc <- unpackProc rator
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
    applyProcedure :: Procedure -> [Ref] -> EvaluateResult
    applyProcedure (Procedure params body savedEnv) rands = do
      pairs <- safeZip params (fmap DenoRef rands)
      valueOf body (extendMany pairs savedEnv)
