module LetRecLang.Data where

import qualified Data.Map   as M
import           Data.Maybe (fromMaybe)

-- | This implementation (together with applySafe') is inefficient,
-- as it build a new closure every time the procedure is retrived.
-- Use lazy evaluation, we can have a better solution
{-
data Environment = EmptyEnv
                 | NormalEnv (M.Map String ExpressedValue) Environment
                 | RecEnv (M.Map String ([String], Expression)) Environment

empty :: Environment
empty = EmptyEnv

initEnvironment :: [(String, ExpressedValue)] -> Environment
initEnvironment items = NormalEnv (M.fromList items) EmptyEnv

extend :: String -> ExpressedValue -> Environment -> Environment
extend param val EmptyEnv =
  initEnvironment [(param, val)]
extend param val (NormalEnv headEnv restEnv) =
  NormalEnv (M.insert param val headEnv) restEnv
extend param val env@RecEnv{} =
  NormalEnv (M.fromList [(param, val)]) env

extendRec :: String -> [String] -> Expression -> Environment -> Environment
extendRec name params procBody env =
  extendRecMany [(name, params, procBody)] env

extendRecMany :: [(String, [String], Expression)] -> Environment -> Environment
extendRecMany lst = RecEnv (M.fromList (fmap func lst))
  where func (name, params, body) = (name, (params, body))

applySafe :: Environment -> String -> Maybe ExpressedValue
applySafe EmptyEnv _ = Nothing
applySafe (NormalEnv headEnv restEnv) name =
  case M.lookup name headEnv of
    Nothing -> applySafe restEnv name
    res     -> res
applySafe env@(RecEnv headEnv restEnv) name =
  case M.lookup name headEnv of
    Nothing             -> applySafe restEnv name
    Just (params, body) -> Just $ ExprProc params body env

extendMany :: [(String, ExpressedValue)] -> Environment -> Environment
extendMany = flip (foldl func)
  where
    func env (var, val) = extend var val env

apply :: Environment -> String -> ExpressedValue
apply env var = fromMaybe
  (error $ "Var " `mappend` var `mappend` " is not in environment!")
  (applySafe env var)
-}

type Environment = M.Map String ExpressedValue

empty :: Environment
empty = M.empty

initEnvironment :: [(String, ExpressedValue)] -> Environment
initEnvironment = M.fromList

extend :: String -> ExpressedValue -> Environment -> Environment
extend = M.insert

extendRec :: String -> [String] -> Expression -> Environment -> Environment
extendRec name params body env = newEnv
  where newEnv = extend name (ExprProc params body newEnv) env

extendRecMany :: [(String, [String], Expression)] -> Environment -> Environment
extendRecMany triples env = newEnv
  where
    newEnv =
      extendMany
      (fmap (\(name, params, body) ->
               (name, ExprProc params body newEnv)) triples)
      env

applySafe :: Environment -> String -> Maybe ExpressedValue
applySafe = flip M.lookup

extendMany :: [(String, ExpressedValue)] -> Environment -> Environment
extendMany = flip (foldl func)
  where
    func env (var, val) = extend var val env

apply :: Environment -> String -> ExpressedValue
apply env var = fromMaybe
  (error $ "Var " `mappend` var `mappend` " is not in environment!")
  (applySafe env var)

data Program = Program Expression
  deriving (Show, Eq)

data Expression =
    ConstExpr ExpressedValue
  | VarExpr String
  | LetExpr [(String, Expression)] Expression
  | LetStarExpr [(String, Expression)] Expression
  | BinOpExpr BinOp Expression Expression
  | UnaryOpExpr UnaryOp Expression
  | CondExpr [(Expression, Expression)]
  | ProcExpr [String] Expression
  | CallExpr Expression [Expression]
  | LetRecExpr [(String, [String], Expression)] Expression
  deriving(Show, Eq)

data BinOp =
  Add | Sub | Mul | Div | Gt | Le | Eq
  deriving(Show, Eq)

data UnaryOp = Minus | IsZero
  deriving(Show, Eq)

data ExpressedValue = ExprNum Integer
                    | ExprBool Bool
                    | ExprProc [String] Expression Environment

instance Show ExpressedValue where
  show (ExprNum i)  = show i
  show (ExprBool b) = show b
  show ExprProc{}   = "<procedure>"

instance Eq ExpressedValue where
  (ExprNum i1) == (ExprNum i2) = i1 == i2
  (ExprBool b1) == (ExprBool b2) = b1 == b2
  _ == _ = False

data DenotedValue = DenoNum Integer
                  | DenoBool Bool
                  | DenoProc [String] [Expression] Environment

instance Show DenotedValue where
  show (DenoNum i)  = show i
  show (DenoBool b) = show b
  show DenoProc{}   = "<procedure>"

instance Eq DenotedValue where
  (DenoNum i1) == (DenoNum i2) = i1 == i2
  (DenoBool b1) == (DenoBool b2) = b1 == b2
  _ == _ = False

