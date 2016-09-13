module NamelessIntp.Data where

import qualified Data.Map   as M
import           Data.Maybe (fromMaybe)

type Try = Either String

type NamelessEnvironment = [ExpressedValue]

empty :: NamelessEnvironment
empty = []

initEnvironment :: [ExpressedValue] -> NamelessEnvironment
initEnvironment = id

extend :: ExpressedValue -> NamelessEnvironment -> NamelessEnvironment
extend = (:)

extendMany :: [ExpressedValue] -> NamelessEnvironment -> NamelessEnvironment
extendMany = mappend

applyForce :: NamelessEnvironment -> Integer -> ExpressedValue
applyForce env lexAddr = fromMaybe
  (error $ "Invalid address: " `mappend` show lexAddr `mappend` "!")
  (apply env lexAddr)

apply :: NamelessEnvironment -> Integer -> Maybe ExpressedValue
apply (x:_) 0  = Just x
apply (_:xs) n = apply xs (n - 1)
apply [] n     = Nothing

type StaticEnvironment = [String]

emptySEnv :: [String]
emptySEnv = []

extendSEnv :: String -> StaticEnvironment -> StaticEnvironment
extendSEnv = (:)

applySEnv :: StaticEnvironment -> String -> Maybe Integer
applySEnv [] _ = Nothing
applySEnv (h:t) sym =
  if h == sym then Just 0 else (+ 1) <$> applySEnv t sym

applySEnvForce :: StaticEnvironment -> String -> Integer
applySEnvForce env var = fromMaybe
  (error $ "Var " `mappend` var `mappend` " is not in static environment!")
  (applySEnv env var)

data NamelessProgram = NamelessProg NamelessExpression
  deriving (Show, Eq)

data NamelessExpression =
    NamelessConstExpr Integer
  | NamelessVarExpr Integer
  | NamelessLetExpr NamelessExpression NamelessExpression
  | NamelessProcExpr NamelessExpression
  | NamelessBinOpExpr BinOp NamelessExpression NamelessExpression
  | NamelessUnaryOpExpr UnaryOp NamelessExpression
  | NamelessIfExpr NamelessExpression NamelessExpression NamelessExpression
  | NamelessCallExpr NamelessExpression NamelessExpression
  | NamelessCondExpr [(NamelessExpression, NamelessExpression)]
  | NamelessLetRecExpr NamelessExpression NamelessExpression
  deriving (Show, Eq)

data Program = Prog Expression
  deriving (Show, Eq)

data Expression =
    ConstExpr Integer
  | VarExpr String
  | LetExpr String Expression Expression
  | BinOpExpr BinOp Expression Expression
  | UnaryOpExpr UnaryOp Expression
  | IfExpr Expression Expression Expression
  | ProcExpr String Expression
  | CallExpr Expression Expression
  | CondExpr [(Expression, Expression)]
  | LetRecExpr String String Expression Expression
  deriving(Show, Eq)

data BinOp =
  Add | Sub | Mul | Div | Gt | Le | Eq
  deriving(Show, Eq)

data UnaryOp = Minus | IsZero
  deriving(Show, Eq)

data NamelessProcedure =
  NamelessProcedure NamelessExpression NamelessEnvironment

instance Show NamelessProcedure where
  show _ = "<procedure>"

data ExpressedValue = ExprNum Integer
                    | ExprBool Bool
                    | ExprProc NamelessProcedure

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
                  | DenoProc NamelessProcedure

instance Show DenotedValue where
  show (DenoNum i)  = show i
  show (DenoBool b) = show b
  show (DenoProc p) = show p

instance Eq DenotedValue where
  (DenoNum i1) == (DenoNum i2) = i1 == i2
  (DenoBool b1) == (DenoBool b2) = b1 == b2
  _ == _ = False

