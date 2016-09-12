module MutablePairs.Data where

import           Control.Monad.Trans.State.Lazy
import qualified Data.Map                       as M
import           Data.Maybe                     (fromMaybe)

type Try = Either String

type Environment = M.Map String DenotedValue

empty :: Environment
empty = M.empty

initEnvironment :: [(String, DenotedValue)] -> Environment
initEnvironment = M.fromList

extend :: String -> DenotedValue -> Environment -> Environment
extend = M.insert

extendRec :: String -> [String] -> Expression -> Environment
          -> StatedTry Environment
extendRec name params body = extendRecMany [(name, params, body)]

extendRecMany :: [(String, [String], Expression)] -> Environment
              -> StatedTry Environment
extendRecMany lst env = do
  refs <- allocMany (length lst)
  let denoVals = fmap DenoRef refs
  let names = fmap (\(n, _, _) -> n) lst
  let newEnv = extendMany (zip names denoVals) env
  extendRecMany' lst refs newEnv
  where
    extendRecMany' [] [] env = return env
    extendRecMany' ((name, params, body):triples) (ref:refs) env = do
      _ <- setRef ref (ExprProc params body env)
      extendRecMany' triples refs env
    allocMany 0 = return []
    allocMany x = do
      ref <- newRef (ExprBool False) -- dummy value false for allocating space
      (ref:) <$> allocMany (x - 1)

applySafe :: Environment -> String -> Maybe DenotedValue
applySafe = flip M.lookup

extendMany :: [(String, DenotedValue)] -> Environment -> Environment
extendMany = flip (foldl func)
  where
    func env (var, val) = extend var val env

apply :: Environment -> String -> DenotedValue
apply env var = fromMaybe
  (error $ "Var " `mappend` var `mappend` " is not in environment!")
  (applySafe env var)

newtype Ref = Ref { addr::Integer } deriving(Show, Eq)

newtype Store = Store { refs::[ExpressedValue] } deriving(Show)

type StatedTry = StateT Store (Either String)

throwError :: String -> StatedTry a
throwError msg = StateT (\s -> Left msg)

initStore :: Store
initStore = Store []

newRef :: ExpressedValue -> StatedTry Ref
newRef val = do
  store <- get
  let refList = refs store
  _ <- put $ Store (val:refList)
  return . Ref . toInteger . length $ refList

deRef :: Ref -> StatedTry ExpressedValue
deRef (Ref r) = do
  store <- get
  let refList = refs store
  findVal r (reverse refList)
  where
    findVal 0 (x:_)  = return x
    findVal 0 []     = throwError "Index out of bound when calling deref!"
    findVal i (_:xs) = findVal (i - 1) xs

setRef :: Ref -> ExpressedValue -> StatedTry ()
setRef ref val = do
  store <- get
  let refList = refs store
  let i = addr ref
  newList <- reverse <$> setRefVal i (reverse refList) val
  put $ Store newList
  return ()
  where
    setRefVal _ [] _       = throwError "Index out of bound when calling setref!"
    setRefVal 0 (_:xs) val = return (val:xs)
    setRefVal i (x:xs) val = (x:) <$> setRefVal (i - 1) xs val
data Program = Prog Expression
  deriving (Show, Eq)

data Expression =
    ConstExpr ExpressedValue
  | VarExpr String
  | LetExpr [(String, Expression)] Expression
  | BinOpExpr BinOp Expression Expression
  | UnaryOpExpr UnaryOp Expression
  | CondExpr [(Expression, Expression)]
  | ProcExpr [String] Expression
  | CallExpr Expression [Expression]
  | LetRecExpr [(String, [String], Expression)] Expression
  | BeginExpr [Expression]
  | AssignExpr String Expression
  | SetDynamicExpr String Expression Expression
  | NewPairExpr Expression Expression
  | GetLeftExpr Expression
  | GetRightExpr Expression
  | SetLeftExpr Expression Expression
  | SetRightExpr Expression Expression
  deriving(Show, Eq)

data BinOp =
  Add | Sub | Mul | Div | Gt | Le | Eq
  deriving(Show, Eq)

data UnaryOp = Minus | IsZero
  deriving(Show, Eq)

data ExpressedValue = ExprNum Integer
                    | ExprBool Bool
                    | ExprProc [String] Expression Environment
                    | ExprPair MutPair

instance Show ExpressedValue where
  show (ExprNum i)  = show i
  show (ExprBool b) = show b
  show (ExprPair p) = show p
  show ExprProc{}   = "<procedure>"

instance Eq ExpressedValue where
  (ExprNum i1) == (ExprNum i2) = i1 == i2
  (ExprBool b1) == (ExprBool b2) = b1 == b2
  (ExprPair p1) == (ExprPair p2) = p1 == p2
  _ == _ = False

data DenotedValue = DenoRef Ref

instance Show DenotedValue where
  show (DenoRef v) = show v

instance Eq DenotedValue where
  DenoRef v1 == DenoRef v2 = v1 == v2

data MutPair = MutPair Ref Ref deriving(Show, Eq)

makePair :: ExpressedValue -> ExpressedValue -> StatedTry MutPair
makePair lVal rVal = do
  lRef <- newRef lVal
  rRef <- newRef rVal
  return $ MutPair lRef rRef

getLeft :: MutPair -> StatedTry ExpressedValue
getLeft (MutPair lRef _) = deRef lRef

getRight :: MutPair -> StatedTry ExpressedValue
getRight (MutPair _ rRef) = deRef rRef

setLeft :: MutPair -> ExpressedValue -> StatedTry ()
setLeft (MutPair lRef _) = setRef lRef

setRight :: MutPair -> ExpressedValue -> StatedTry ()
setRight (MutPair _ rRef) = setRef rRef
