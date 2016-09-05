module ProcLang.Data where

import qualified Data.Map   as M
import           Data.Maybe (fromMaybe)

type Environment = M.Map String ExpressedValue

empty :: Environment
empty = M.empty

initEnvironment :: [(String, ExpressedValue)] -> Environment
initEnvironment = M.fromList

extend :: String -> ExpressedValue -> Environment -> Environment
extend = M.insert

extendMany :: [(String, ExpressedValue)] -> Environment -> Environment
extendMany = flip (foldl func)
  where
    func env (var, val) = extend var val env

apply :: Environment -> String -> ExpressedValue
apply env var = fromMaybe
  (error $ "Var " `mappend` var `mappend` " is not in environment!")
  (applySafe env var)

applySafe :: Environment -> String -> Maybe ExpressedValue
applySafe = flip M.lookup

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
                  | DenoProc [String] Expression Environment

instance Show DenotedValue where
  show (DenoNum i)  = show i
  show (DenoBool b) = show b
  show DenoProc{}   = "<procedure>"

instance Eq DenotedValue where
  (DenoNum i1) == (DenoNum i2) = i1 == i2
  (DenoBool b1) == (DenoBool b2) = b1 == b2
  _ == _ = False

