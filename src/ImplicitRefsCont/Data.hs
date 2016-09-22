module ImplicitRefsCont.Data where

import           Control.Monad.Except
import           Data.IORef
import qualified Data.Map             as M
import           Data.Maybe           (fromMaybe)
import qualified Text.Megaparsec      as Mega

type Environment = M.Map String DenotedValue

empty :: Environment
empty = M.empty

initEnvironment :: [(String, DenotedValue)] -> Environment
initEnvironment = M.fromList

extend :: String -> DenotedValue -> Environment -> Environment
extend = M.insert

extendRec :: Store -> String -> [String] -> Expression -> Environment
          -> IOTry Environment
extendRec store name params body = extendRecMany store [(name, params, body)]

extendRecMany :: Store -> [(String, [String], Expression)] -> Environment
              -> IOTry Environment
extendRecMany store lst env = do
  refs <- allocMany (length lst)
  let denoVals = fmap DenoRef refs
  let names = fmap (\(n, _, _) -> n) lst
  let newEnv = extendMany (zip names denoVals) env
  extendRecMany' lst refs newEnv
  where
    extendRecMany' [] [] env = return env
    extendRecMany' ((name, params, body):triples) (ref:refs) env = do
      setRef store ref (ExprProc $ Procedure params body env)
      extendRecMany' triples refs env
    allocMany 0 = return []
    allocMany x = do
      ref <- newRef store (ExprBool False) -- dummy value false for allocating space
      (ref:) <$> allocMany (x - 1)

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

newtype Ref = Ref { addr::Integer } deriving (Show, Eq)

type Store = IORef [ExpressedValue]

type Try = Either LangError

type IOTry = ExceptT LangError IO

liftTry :: Try a -> IOTry a
liftTry (Left  err) = throwError err
liftTry (Right val) = return val

initStore :: IO Store
initStore = newIORef []

newRef :: Store -> ExpressedValue -> IOTry Ref
newRef store val = do
  vals <- liftIO $ readIORef store
  liftIO $ atomicWriteIORef store (val:vals)
  return . Ref . toInteger . length $ vals

deRef :: Store -> Ref -> IOTry ExpressedValue
deRef store (Ref r) = do
  vals <- liftIO $ readIORef store
  findVal r (reverse vals)
  where
    findVal :: Integer -> [ExpressedValue] -> IOTry ExpressedValue
    findVal 0 (x:_)  = return x
    findVal 0 []     = throwError $ IndexOutOfBound "deref"
    findVal i (_:xs) = findVal (i - 1) xs

setRef :: Store -> Ref -> ExpressedValue -> IOTry ()
setRef store ref val = do
  vals <- liftIO $ readIORef store
  newVals <- reverse <$> setRefVal (addr ref) (reverse vals) val
  liftIO $ writeIORef store newVals
  where
    setRefVal :: Integer -> [ExpressedValue] -> ExpressedValue
              -> IOTry [ExpressedValue]
    setRefVal 0 (_:xs) val = return (val:xs)
    setRefVal _ [] _       = throwError $ IndexOutOfBound "setref"
    setRefVal i (x:xs) val = (x:) <$> setRefVal (i - 1) xs val

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
  | BeginExpr [Expression]
  | AssignExpr String Expression
  deriving (Show, Eq)

data BinOp =
  Add | Sub | Mul | Div | Gt | Le | Eq
  deriving (Show, Eq)

data UnaryOp = Minus | IsZero
  deriving (Show, Eq)

data Procedure = Procedure [String] Expression Environment

instance Show Procedure where
  show _ = "<procedure>"

data ExpressedValue = ExprNum Integer
                    | ExprBool Bool
                    | ExprProc Procedure
                    | ExprRef Ref

instance Show ExpressedValue where
  show (ExprNum i)   = show i
  show (ExprBool b)  = show b
  show (ExprProc p)  = show p
  show (ExprRef ref) = show ref

instance Eq ExpressedValue where
  (ExprNum i1) == (ExprNum i2) = i1 == i2
  (ExprBool b1) == (ExprBool b2) = b1 == b2
  (ExprRef ref1) == (ExprRef ref2) = ref1 == ref2
  _ == _ = False

data DenotedValue = DenoRef Ref

instance Show DenotedValue where
  show (DenoRef v) = show v

instance Eq DenotedValue where
  DenoRef v1 == DenoRef v2 = v1 == v2

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

type UnaryFunc = ExpressedValue -> IOTry ExpressedValue
type BinFunc = ExpressedValue -> ExpressedValue -> IOTry ExpressedValue

data Continuation =
    EndCont
  | UnaryOpCont UnaryFunc Continuation
  | BinOpCont1 BinFunc Expression Environment Continuation
  | BinOpCont2 BinFunc ExpressedValue Continuation
  | IfCont Expression Expression Environment Continuation
  | LetCont String [(String, Expression)] [(String, DenotedValue)] Expression Environment Continuation
  | RatorCont [Expression] Environment Continuation
  | RandCont [Expression] ExpressedValue [ExpressedValue] Environment Continuation
  | AssignCont String Environment Continuation
  | BeginCont [Expression] Environment Continuation
