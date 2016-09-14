module ContinuationPassing.Data where

import qualified Data.Map   as M
import           Data.Maybe (fromMaybe)

type Try = Either String

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

newtype Continuation = Continuation (ExpressedValue -> Try ExpressedValue)

applyCont :: Continuation -> ExpressedValue -> Try ExpressedValue
applyCont (Continuation func) = func

endCont :: Continuation
endCont = Continuation return

extendCont :: (ExpressedValue -> Try ExpressedValue) -> Continuation
           -> Continuation
extendCont func cont = Continuation newCont
  where newCont val = applyCont cont val >>= func

data Program = Prog Expression
  deriving (Show, Eq)

data Expression =
    ConstExpr ExpressedValue
  | VarExpr String
  | LetExpr [(String, Expression)] Expression
  | BinOpExpr BinOp Expression Expression
  | UnaryOpExpr UnaryOp Expression
  | IfExpr Expression Expression Expression
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

