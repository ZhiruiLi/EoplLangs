module MutablePairs.Evaluator
( valueOf
, run
, eval
, evalProgram
) where

import           Control.Applicative            ((<|>))
import           Control.Monad.Trans.State.Lazy (evalStateT)
import           MutablePairs.Data
import           MutablePairs.Parser

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
valueOf (NewPairExpr left right) env   = evalNewPairExpr left right env
valueOf (GetLeftExpr expr) env         = evalGetLeftExpr expr env
valueOf (GetRightExpr expr) env        = evalGetRightExpr expr env
valueOf (SetLeftExpr pair expr) env    = evalSetLeftExpr pair expr env
valueOf (SetRightExpr pair expr) env   = evalSetRightExpr pair expr env

evalNewPairExpr :: Expression -> Expression -> Environment -> EvaluateResult
evalNewPairExpr left right env = do
  lVal <- valueOf left env
  rVal <- valueOf right env
  pair <- makePair lVal rVal
  return $ ExprPair pair

unpackPair :: ExpressedValue -> String -> StatedTry MutPair
unpackPair (ExprPair pair) _ = return pair
unpackPair notPair name = throwError $
  concat [ name, "require pair value, but got: ", show notPair ]

evalGetLeftExpr :: Expression -> Environment -> EvaluateResult
evalGetLeftExpr expr env = do
  val <- valueOf expr env
  pair <- unpackPair val "getleft"
  getLeft pair

evalGetRightExpr :: Expression -> Environment -> EvaluateResult
evalGetRightExpr expr env = do
  val <- valueOf expr env
  pair <- unpackPair val "getright"
  getRight pair

evalSetLeftExpr :: Expression -> Expression -> Environment -> EvaluateResult
evalSetLeftExpr pairExpr expr env = do
  pairVal <- valueOf pairExpr env
  pair <- unpackPair pairVal "setleft"
  newVal <- valueOf expr env
  setLeft pair newVal
  return $ ExprBool False

evalSetRightExpr :: Expression -> Expression -> Environment -> EvaluateResult
evalSetRightExpr pairExpr expr env = do
  pairVal <- valueOf pairExpr env
  pair <- unpackPair pairVal "setright"
  newVal <- valueOf expr env
  _ <- setRight pair newVal
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
  Nothing            -> throwError $ "Not in scope: " ++ show name

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
tryFindOp op = tryFind ("Unknown operator: " ++ show op) op

evalBinOpExpr :: BinOp -> Expression -> Expression -> Environment
              -> EvaluateResult
evalBinOpExpr op expr1 expr2 env = do
  v1 <- valueOf expr1 env
  v2 <- valueOf expr2 env
  numToNum v1 v2 <|> numToBool v1 v2  <|> boolToBool v1 v2
  where
    findOpFrom = tryFindOp op
    unpackN = unpackNum $ "binary operation " ++ show op
    unpackB = unpackBool $ "binary operation " ++ show op
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
    unpackN = unpackNum $ "unary operation " ++ show op
    unpackB = unpackBool $ "unary operation " ++ show op
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
  proc <- unpackProc rator
  rands <- evalExpressionList randExprs env
  applyProcedure proc rands
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
