module LetLang.Evaluator
( valueOf
, run
, eval
, evalProgram
) where

import           Control.Applicative ((<|>))
import           LetLang.Data
import           LetLang.Parser

type EvaluateResult = Either String ExpressedValue

liftMaybe :: String -> Maybe a -> Try a
liftMaybe _ (Just x) = return x
liftMaybe y Nothing  = Left y

run :: String -> EvaluateResult
run input = parseProgram input >>= evalProgram

eval :: Expression -> EvaluateResult
eval = flip valueOf empty

evalProgram :: Program -> EvaluateResult
evalProgram (Prog expr) = eval expr

valueOf :: Expression -> Environment -> EvaluateResult
valueOf (ConstExpr x) _   = Right x
valueOf (VarExpr var) env = case apply env var of
  Nothing  -> Left $ "Not in scope: " `mappend` var
  Just val -> Right val
-- begin operate on list
valueOf EmptyListExpr _ = Right $ ExprList []
valueOf (BinOpExpr Cons expr1 expr2) env =
  case (valueOf expr1 env, valueOf expr2 env) of
    (msg@(Left _), _) -> msg
    (_, msg@(Left _)) -> msg
    (Right v, Right (ExprList lst)) -> Right $ ExprList (v:lst)
    (_, Right v) -> Left $
      "The second operand of '" `mappend` show Cons `mappend`
      "' should be list, but got: " `mappend` show v
valueOf (UnaryOpExpr Car expr) env =
  case valueOf expr env of
    msg@(Left _) -> msg
    Right (ExprList []) -> Left "Could not applyForce 'car' on empty list"
    Right (ExprList (v:_)) -> Right v
    Right v -> Left $
      "Operand of '" `mappend` show Car `mappend`
      "' should be list, but got: " `mappend` show v
valueOf (UnaryOpExpr Cdr expr) env =
  case valueOf expr env of
    msg@(Left _) -> msg
    Right (ExprList []) -> Left "Could not applyForce 'cdr' on empty list"
    Right (ExprList (_:t)) -> Right $ ExprList t
    Right v -> Left $
      "Operand of '" `mappend` show Cdr `mappend`
      "' should be list, but got: " `mappend` show v
valueOf (ListExpr es) env = buildList es env
-- end operate on list
valueOf (BinOpExpr op expr1 expr2) env = evalBinOpExpr op expr1 expr2 env
valueOf (UnaryOpExpr op expr) env = evalUnaryOpExpr op expr env
valueOf (CondExpr pairs) env = evalCondExpr pairs env
valueOf (LetExpr bindings body) env = evalLetExpr bindings body env
valueOf (LetStarExpr bindings body) env = evalLetStarExpr bindings body env

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

buildList :: [Expression] -> Environment -> EvaluateResult
buildList es env = case collect of
  Left msg   -> Left msg
  Right ress -> Right . ExprList $ reverse ress
  where
    collector :: Either String [ExpressedValue]
              -> EvaluateResult
              -> Either String [ExpressedValue]
    collector acc res = case (acc, res) of
      (Left msg, _)            -> Left msg
      (_, Left msg)            -> Left msg
      (Right vAcc, Right vRes) -> Right $ vRes:vAcc
    results :: [EvaluateResult]
    results = flip valueOf env <$> es
    collect :: Either String [ExpressedValue]
    collect = foldl collector (Right []) results

evalCondExpr :: [(Expression, Expression)] -> Environment -> EvaluateResult
evalCondExpr [] _ = Left "No predicate is true"
evalCondExpr ((e1, e2):pairs) env = case valueOf e1 env of
  Left msg -> Left msg
  Right (ExprBool True) -> valueOf e2 env
  Right (ExprBool False) -> evalCondExpr pairs env
  Right v -> Left $
    "Predicate expression should be boolean, but got: "
    `mappend` show v

evalLetExpr :: [(String, Expression)] -> Expression -> Environment -> EvaluateResult
evalLetExpr bindings body env = case evaledBindings of
  Left msg    -> Left msg
  Right pairs -> valueOf body $ extendMany pairs env
  where
    func :: Either String [(String, ExpressedValue)]
         -> (String, Expression)
         -> Either String [(String, ExpressedValue)]
    func left@(Left _) _ = left
    func (Right pairs) (var, expr) = case valueOf expr env of
      Left msg  -> Left msg
      Right val -> Right $ (var, val):pairs
    evaledBindings :: Either String [(String, ExpressedValue)]
    evaledBindings = case foldl func (Right []) bindings of
      left@(Left _) -> left
      Right pairs   -> Right $ reverse pairs

evalLetStarExpr :: [(String, Expression)] -> Expression -> Environment -> EvaluateResult
evalLetStarExpr [] body env = valueOf body env
evalLetStarExpr ((var, expr):pairs) body env = case valueOf expr env of
  Left msg  -> Left msg
  Right val -> evalLetStarExpr pairs body (extend var val env)
