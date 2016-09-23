module ProcLang.Evaluator
( valueOf
, run
, eval
, evalProgram
) where

import           Control.Applicative ((<|>))
import           ProcLang.Data
import           ProcLang.Parser

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
valueOf (BinOpExpr op expr1 expr2) env  = evalBinOpExpr op expr1 expr2 env
valueOf (UnaryOpExpr op expr) env       = evalUnaryOpExpr op expr env
valueOf (CondExpr pairs) env            = evalCondExpr pairs env
valueOf (LetExpr bindings body) env     = evalLetExpr bindings body env
valueOf (LetStarExpr bindings body) env = evalLetStarExpr bindings body env
valueOf (ProcExpr params body) env      = evalProcExpr params body env
valueOf (CallExpr rator rand) env       = evalCallExpr rator rand env

evalConstExpr :: ExpressedValue -> EvaluateResult
evalConstExpr = return

evalVarExpr :: String -> Environment -> EvaluateResult
evalVarExpr var env =
  liftMaybe ("Not in scope: " `mappend` var) (apply env var)

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

unpackNum :: String -> ExpressedValue -> Try Integer
unpackNum _ (ExprNum n) = return n
unpackNum caller notNum = Left $ concat [
  caller, ": Unpacking a not number value: ", show notNum ]

unpackBool :: String -> ExpressedValue -> Try Bool
unpackBool _ (ExprBool b) = return b
unpackBool caller notBool = Left $ concat [
  caller, ": Unpacking a not boolean value: ", show notBool ]

tryFind :: Eq a => String -> a -> [(a, b)] -> Try b
tryFind err x pairs = liftMaybe err (lookup x pairs)

tryFindOp :: (Eq a, Show a) => a -> [(a, b)] -> Try b
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
evalCondExpr [] _ = Left "No predicate is true"
evalCondExpr ((e1, e2):pairs) env = do
  v <- valueOf e1 env
  case v of
    ExprBool True -> valueOf e2 env
    ExprBool False -> evalCondExpr pairs env
    _ -> Left $
      "Predicate expression should be boolean, but got: "
      `mappend` show v

evalLetExpr :: [(String, Expression)] -> Expression -> Environment
            -> EvaluateResult
evalLetExpr bindings body env = do
  bindVals <- evaledBindings
  valueOf body $ extendMany bindVals env
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
  evalLetStarExpr pairs body (extend var val env)

evalProcExpr :: [String] -> Expression -> Environment -> EvaluateResult
evalProcExpr params body env = return . ExprProc $ Procedure params body env

evalCallExpr :: Expression -> [Expression] -> Environment -> EvaluateResult
evalCallExpr rator rand env = do
  rator <- valueOf rator env
  proc <- unpackProc rator
  args <- maybeArgs
  applyProcedure proc args
  where
    unpackProc :: ExpressedValue -> Try Procedure
    unpackProc (ExprProc proc) = Right proc
    unpackProc notProc = Left $
      "Operator of call expression should be procedure, but got: "
      `mappend` show notProc
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
                    -> [ExpressedValue]
                    -> [String]
                    -> EvaluateResult
    applyProcedure' [] body env [] _ = valueOf body env
    applyProcedure' params _ _ [] _ =
      Left $ "Too many parameters: " `mappend` show params
    applyProcedure' [] _ _ args _ =
      Left $ "Too many arguments: " `mappend` show args
    applyProcedure' (p:ps) body env (a:as) usedParams =
      if p `elem` usedParams
        then Left $ "Parameter name conflict: " `mappend` p
        else applyProcedure' ps body (extend p a env) as (p:usedParams)
