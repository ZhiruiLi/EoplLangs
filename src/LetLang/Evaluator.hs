module LetLang.Evaluator
( valueOf
, run
, eval
, evalProgram
) where

import           LetLang.Data
import           LetLang.Parser

type EvaluateResult = Either String ExpressedValue

run :: String -> EvaluateResult
run input = parseProgram input >>= evalProgram

eval :: Expression -> EvaluateResult
eval = flip valueOf empty

evalProgram :: Program -> EvaluateResult
evalProgram (Prog expr) = eval expr

valueOf :: Expression -> Environment -> EvaluateResult
valueOf (ConstExpr x) _   = Right x
valueOf (VarExpr var) env = case applySafe env var of
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
    Right (ExprList []) -> Left "Could not apply 'car' on empty list"
    Right (ExprList (v:_)) -> Right v
    Right v -> Left $
      "Operand of '" `mappend` show Car `mappend`
      "' should be list, but got: " `mappend` show v
valueOf (UnaryOpExpr Cdr expr) env =
  case valueOf expr env of
    msg@(Left _) -> msg
    Right (ExprList []) -> Left "Could not apply 'cdr' on empty list"
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

evalBinOpExpr :: BinOp
              -> Expression
              -> Expression
              -> Environment
              -> EvaluateResult
evalBinOpExpr op expr1 expr2 env =
  case (wrapVal1, wrapVal2) of
    (msg@(Left _), _) -> msg
    (_, msg@(Left _)) -> msg
    (Right val1, Right val2) ->
      case ( lookup op binNumToNumOpMap
           , lookup op binNumToBoolOpMap
           , lookup op binBoolOpMap
           ) of
        (Just func, _, _) -> case (val1, val2) of
          (ExprNum n1, ExprNum n2) -> Right . ExprNum $ func n1 n2
          (a, b)                   -> opError "number" op a b
        (_, Just func, _) -> case (val1, val2) of
          (ExprNum n1, ExprNum n2) -> Right . ExprBool $ func n1 n2
          (a, b)                   -> opError "number" op a b
        (_, _, Just func) -> case (val1, val2) of
          (ExprBool b1, ExprBool b2) -> Right . ExprBool $ func b1 b2
          (a, b)                     -> opError "boolean value" op a b
        _ -> invalidOpError op
  where
    wrapVal1 = valueOf expr1 env
    wrapVal2 = valueOf expr2 env
    opError typeName op a b = Left $ concat
      [ "Operands of binary ", show op, " operator "
      , "should both be ", typeName, "s, but got: "
      , show a, " and ", show b
      ]

invalidOpError op = error $ "Invalid operator: " `mappend` show op

evalUnaryOpExpr :: UnaryOp
                -> Expression
                -> Environment
                -> EvaluateResult
evalUnaryOpExpr op expr env =
  case valueOf expr env of
    msg@(Left _) -> msg
    (Right val) ->
      case ( lookup op unaryNumToNumOpMap
           , lookup op unaryNumToBoolOpMap
           , lookup op unaryBoolOpMap
           ) of
        (Just func, _, _) -> case val of
          (ExprNum n) -> Right . ExprNum $ func n
          _           -> opError "number" op val
        (_, Just func, _) -> case val of
          (ExprNum n) -> Right . ExprBool $ func n
          _           -> opError "number" op val
        (_, _, Just func) -> case val of
          (ExprBool b) -> Right . ExprBool $ func b
          _            -> opError "boolean value" op val
        _ -> invalidOpError op
  where
    opError typeName op val = Left $ concat
      [ "Operand of ", show op , " operator "
      , "should be ", typeName, ", but got: "
      , show val
      ]

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
