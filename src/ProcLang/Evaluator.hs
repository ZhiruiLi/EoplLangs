module ProcLang.Evaluator
( valueOf
, run
, eval
, evalProgram
) where

import           ProcLang.Data
import           ProcLang.Parser

type EvaluateResult = Either String ExpressedValue

run :: String -> EvaluateResult
run input = parseProgram input >>= evalProgram

eval :: Expression -> EvaluateResult
eval = flip valueOf empty

evalProgram :: Program -> EvaluateResult
evalProgram (Program expr) = eval expr

valueOf :: Expression -> Environment -> EvaluateResult
valueOf (ConstExpr x) _   = Right x
valueOf (VarExpr var) env = case applySafe env var of
  Nothing  -> Left $ "Not in scope: " `mappend` var
  Just val -> Right val
valueOf (BinOpExpr op expr1 expr2) env = evalBinOpExpr op expr1 expr2 env
valueOf (UnaryOpExpr op expr) env = evalUnaryOpExpr op expr env
valueOf (CondExpr pairs) env = evalCondExpr pairs env
valueOf (LetExpr bindings body) env = evalLetExpr bindings body env
valueOf (LetStarExpr bindings body) env = evalLetStarExpr bindings body env
valueOf (ProcExpr params body) env = Right $ ExprProc params body env
valueOf (CallExpr rator rand) env = evalCallExpr rator rand env

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
    opError typeName op a b = Left $ unlines
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
    opError typeName op val = Left $ unlines
      [ "Operand of ", show op , " operator "
      , "should be ", typeName, ", but got: "
      , show val
      ]

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

evalCallExpr :: Expression -> [Expression] -> Environment -> EvaluateResult
evalCallExpr rator rand env = case valueOf rator env of
  Left msg -> Left msg
  Right (ExprProc params body savedEnv) -> case maybeArgs of
    Left msg   -> Left msg
    Right args -> applyProcedure params body savedEnv args
  Right noProc -> Left $
    "Operator of call expression should be procedure, but got: "
    `mappend` show noProc
  where
    func :: Either String [ExpressedValue]
         -> Either String ExpressedValue
         -> Either String [ExpressedValue]
    func acc maybeArg = case (acc, maybeArg) of
      (Right args, Right arg) -> Right (arg : args)
      (Left msg, _)           -> Left msg
      (_, Left msg)           -> Left msg
    maybeArgs :: Either String [ExpressedValue]
    maybeArgs = reverse <$>
      foldl func (Right []) (fmap (`valueOf` env) rand)
    applyProcedure :: [String] -> Expression -> Environment
                   -> [ExpressedValue]
                   -> EvaluateResult
    applyProcedure params body savedEnv args =
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

