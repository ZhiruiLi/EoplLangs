module ExplicitRefs.Data where

import           Control.Monad.Trans.State.Lazy
import qualified Data.Map                       as M
import           Data.Maybe                     (fromMaybe)

type Try = Either String

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
  | NewRefExpr Expression
  | DeRefExpr Expression
  | SetRefExpr Expression Expression
  | ListExpr [Expression]
  deriving(Show, Eq)

data BinOp =
  Add | Sub | Mul | Div | Gt | Le | Eq
  deriving(Show, Eq)

data UnaryOp = Minus | IsZero
  deriving(Show, Eq)

data ExpressedValue = ExprNum Integer
                    | ExprBool Bool
                    | ExprProc [String] Expression Environment
                    | ExprRef Ref
                    | ExprList [ExpressedValue]

instance Show ExpressedValue where
  show (ExprNum i)    = show i
  show (ExprBool b)   = show b
  show ExprProc{}     = "<procedure>"
  show (ExprRef v)    = show v
  show (ExprList lst) = showValList lst

showValList :: [ExpressedValue] -> String
showValList lst = concat ["(", body lst ,")"]
  where
    bodyTail = concatMap (\val -> " " `mappend` show val)
    body []     = ""
    body [x]    = show x
    body (x:xs) = show x `mappend` bodyTail xs

instance Eq ExpressedValue where
  (ExprNum i1) == (ExprNum i2) = i1 == i2
  (ExprBool b1) == (ExprBool b2) = b1 == b2
  (ExprRef v1) == (ExprRef v2) = v1 == v2
  (ExprList l1) == (ExprList l2) = l1 == l2
  _ == _ = False

data DenotedValue = DenoVal ExpressedValue

instance Show DenotedValue where
  show (DenoVal v) = show v

instance Eq DenotedValue where
  DenoVal v1 == DenoVal v2 = v1 == v2

