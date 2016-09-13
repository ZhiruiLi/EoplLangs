module LetLang.Data where

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

applyForce :: Environment -> String -> ExpressedValue
applyForce env var = fromMaybe
  (error $ "Var " `mappend` var `mappend` " is not in environment!")
  (apply env var)

apply :: Environment -> String -> Maybe ExpressedValue
apply = flip M.lookup

data Program = Prog Expression
  deriving (Show, Eq)

data Expression =
    ConstExpr ExpressedValue
  | VarExpr String
  | LetExpr [(String, Expression)] Expression
  | LetStarExpr [(String, Expression)] Expression
  | BinOpExpr BinOp Expression Expression
  | UnaryOpExpr UnaryOp Expression
  | EmptyListExpr
  | ListExpr [Expression]
  | CondExpr [(Expression, Expression)]
  deriving(Show, Eq)

data BinOp =
  Add | Sub | Mul | Div | Gt | Le | Eq | Cons
  deriving(Show, Eq)

data UnaryOp = Car | Cdr | Minus | IsZero
  deriving(Show, Eq)

data DenotedValue = DenoNum Integer
                  | DenoBool Bool
  deriving (Eq)

instance Show DenotedValue where
  show (DenoNum i)  = show i
  show (DenoBool b) = show b

data ExpressedValue = ExprNum Integer
                    | ExprBool Bool
                    | ExprList [ExpressedValue]
  deriving (Eq)

instance Show ExpressedValue where
  show (ExprNum i)    = show i
  show (ExprBool b)   = show b
  show (ExprList lst) = showExprList lst

showExprList :: [ExpressedValue] -> String
showExprList []     =             "()"
showExprList [h] = "(" `mappend` show h `mappend` ")"
showExprList (h:t) = "(" `mappend` show h `mappend` showTail t `mappend` ")"
  where
    showTail = foldr (\h -> mappend (" " `mappend` show h)) ""
