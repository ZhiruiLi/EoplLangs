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

apply :: NamelessEnvironment -> Integer -> ExpressedValue
apply env lexAddr = fromMaybe
  (error $ "Invalid address: " `mappend` show lexAddr `mappend` "!")
  (applySafe env lexAddr)

applySafe :: NamelessEnvironment -> Integer -> Maybe ExpressedValue
applySafe (x:_) 0  = Just x
applySafe (_:xs) n = applySafe xs (n - 1)
applySafe [] n     = Nothing

type StaticEnvironment = [String]

emptySEnv :: [String]
emptySEnv = []

extendSEnv :: String -> StaticEnvironment -> StaticEnvironment
extendSEnv = (:)

applySEnvSafe :: StaticEnvironment -> String -> Maybe Integer
applySEnvSafe [] _ = Nothing
applySEnvSafe (h:t) sym =
  if h == sym then Just 0 else (+ 1) <$> applySEnvSafe t sym

applySEnv :: StaticEnvironment -> String -> Integer
applySEnv env var = fromMaybe
  (error $ "Var " `mappend` var `mappend` " is not in static environment!")
  (applySEnvSafe env var)

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

