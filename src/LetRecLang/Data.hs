module LetRecLang.Data where

import qualified Data.Map        as M
import           Data.Maybe      (fromMaybe)
import qualified Text.Megaparsec as Mega

type Try = Either LangError

throwError :: LangError -> Try a
throwError = Left

-- | This implementation (together with apply') is inefficient,
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

apply :: Environment -> String -> Maybe ExpressedValue
apply EmptyEnv _ = Nothing
apply (NormalEnv headEnv restEnv) name =
  case M.lookup name headEnv of
    Nothing -> apply restEnv name
    res     -> res
apply env@(RecEnv headEnv restEnv) name =
  case M.lookup name headEnv of
    Nothing             -> apply restEnv name
    Just (params, body) -> Just $ ExprProc params body env

extendMany :: [(String, ExpressedValue)] -> Environment -> Environment
extendMany = flip (foldl func)
  where
    func env (var, val) = extend var val env

applyForce :: Environment -> String -> ExpressedValue
applyForce env var = fromMaybe
  (error $ "Var " `mappend` var `mappend` " is not in environment!")
  (apply env var)
-}

type Environment = M.Map String DenotedValue

empty :: Environment
empty = M.empty

initEnvironment :: [(String, DenotedValue)] -> Environment
initEnvironment = M.fromList

extend :: String -> DenotedValue -> Environment -> Environment
extend = M.insert

extendRec :: String -> [String] -> Expression -> Environment -> Environment
extendRec name params body env = newEnv
  where newEnv = extend name (DenoProc $ Procedure params body newEnv) env

extendRecMany :: [(String, [String], Expression)] -> Environment -> Environment
extendRecMany triples env = newEnv
  where
    newEnv =
      extendMany
      (fmap (\(name, params, body) ->
               (name, DenoProc $ Procedure params body newEnv)) triples)
      env

apply :: Environment -> String -> Maybe DenotedValue
apply = flip M.lookup

extendMany :: [(String, DenotedValue)] -> Environment -> Environment
extendMany = flip (foldl func)
  where
    func env (var, val) = extend var val env

applyForce :: Environment -> String -> DenotedValue
applyForce env var = fromMaybe
  (error $ "Var " `mappend` var `mappend` " is not in environment!")
  (apply env var)

data Program = Prog Expression
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

data Procedure = Procedure [String] Expression Environment

instance Show Procedure where
  show _ = "<procedure>"

data ExpressedValue = ExprNum Integer
                    | ExprBool Bool
                    | ExprProc Procedure

instance Show ExpressedValue where
  show (ExprNum i)  = show i
  show (ExprBool b) = show b
  show (ExprProc p) = show p

instance Eq ExpressedValue where
  (ExprNum i1) == (ExprNum i2) = i1 == i2
  (ExprBool b1) == (ExprBool b2) = b1 == b2
  _ == _ = False

data DenotedValue = DenoNum Integer
                  | DenoBool Bool
                  | DenoProc Procedure

instance Show DenotedValue where
  show (DenoNum i)  = show i
  show (DenoBool b) = show b
  show (DenoProc p) = show p

instance Eq DenotedValue where
  (DenoNum i1) == (DenoNum i2) = i1 == i2
  (DenoBool b1) == (DenoBool b2) = b1 == b2
  _ == _ = False

data LangError =
    ParseError (Mega.ParseError (Mega.Token String) Mega.Dec)
  | TypeMismatch String ExpressedValue
  | IndexOutOfBound String
  | ArgNumMismatch Integer [ExpressedValue]
  | UnknownOperator String
  | UnboundVar String
  | RuntimeError String
  | DefaultError String
  deriving (Show, Eq)

unpackNum :: ExpressedValue -> Try Integer
unpackNum (ExprNum n) = return n
unpackNum notNum      = throwError $ TypeMismatch "number" notNum

unpackBool :: ExpressedValue -> Try Bool
unpackBool (ExprBool b) = return b
unpackBool notBool      = throwError $ TypeMismatch "boolean" notBool

unpackProc :: ExpressedValue -> Try Procedure
unpackProc (ExprProc proc) = Right proc
unpackProc notProc         = throwError $ TypeMismatch "procedure" notProc
