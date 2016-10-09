module SimpleModule.Data where

import           Control.Monad.Except
import qualified Data.List            as L
import qualified Data.Map             as M
import           Data.Maybe           (fromMaybe)
import qualified Text.Megaparsec      as Mega

type Try = Either LangError

type GeneralEnv a = [EnvItem a]

data EnvItem a = NamedEnv String (M.Map String a)
               | UnnamedEnv (M.Map String a)

type Environment = GeneralEnv DenotedValue

type TypeEnvironment = GeneralEnv Type

type EnvTry a = Either EnvError a

data EnvError = NamedEnvNotFound String
              | NamedEnvKeyNotFound String String
              | UnnamedEnvKeyNotFound String
              deriving (Show, Eq)

empty :: GeneralEnv a
empty = []

initEnvironment :: [(String, a)] -> GeneralEnv a
initEnvironment lst = [UnnamedEnv $ M.fromList lst]

extend :: String -> a -> GeneralEnv a -> GeneralEnv a
extend k v []                  = initEnvironment [(k, v)]
extend k v (UnnamedEnv e : es) = (UnnamedEnv $ M.insert k v e) : es
extend k v es                  = (UnnamedEnv $ M.singleton k v) : es

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

extendMany :: [(String, a)] -> GeneralEnv a -> GeneralEnv a
extendMany = flip (foldl func)
  where
    func env (var, val) = extend var val env

addNamed :: String -> [(String, a)] -> GeneralEnv a -> GeneralEnv a
addNamed name binds env = NamedEnv name (M.fromList binds) : env

hasName :: GeneralEnv a -> String -> Bool
hasName [] _                   = False
hasName (NamedEnv n' _ : es) n | n == n' = True
hasName (_ : es) n             = es `hasName` n

extendNamed :: String -> String -> a -> GeneralEnv a -> GeneralEnv a
extendNamed mName k v env = extendNamed' env []
  where
    extendNamed' [] acc = addNamed mName [(k, v)] (reverse acc)
    extendNamed' (NamedEnv n m : es) acc | n == mName =
      reverse (reverse es `mappend` (NamedEnv n (M.insert k v m) : acc))
    extendNamed' (e : es) acc = extendNamed' es (e : acc)

apply :: GeneralEnv a -> String -> EnvTry a
apply [] k                  = throwError $ UnnamedEnvKeyNotFound k
apply (NamedEnv _ _ : es) k = apply es k
apply (UnnamedEnv e : es) k = case M.lookup k e of
  Nothing -> apply es k
  Just x  -> return x

applyNamed :: GeneralEnv a -> String -> String -> EnvTry a
applyNamed [] name k = throwError $ NamedEnvNotFound name
applyNamed (UnnamedEnv _ : es) name k = applyNamed es name k
applyNamed (NamedEnv n e : es) name k =
  if n == name
    then case M.lookup k e of
           Nothing -> throwError $ NamedEnvKeyNotFound name k
           Just x  -> return x
    else applyNamed es name k

data Program = Prog [ModuleDef] Expression
  deriving (Show, Eq)

data Type = TypeInt | TypeBool | TypeProc [Type] Type deriving (Eq)

instance Show Type where
  show TypeInt = "int"
  show TypeBool = "bool"
  show (TypeProc argTypes resType) =
    let argNames = fmap show argTypes
        sepComma = L.intersperse ", " argNames
        argStr = concat sepComma
    in  concat [ "(", "(", argStr, ")", " -> ", show resType, ")" ]

data Expression =
    ConstExpr ExpressedValue
  | VarExpr String
  | LetExpr [(String, Expression)] Expression
  | BinOpExpr BinOp Expression Expression
  | UnaryOpExpr UnaryOp Expression
  | CondExpr [(Expression, Expression)]
  | ProcExpr [(String, Type)] Expression
  | CallExpr Expression [Expression]
  | LetRecExpr [(Type, String, [(String, Type)], Expression)] Expression
  | QualifiedVarExpr String String
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
  | TypeCheckerError TypeError
  -- | TypeMismatch String ExpressedValue
  | IndexOutOfBound String
  | ArgNumMismatch Integer [ExpressedValue]
  -- | UnknownOperator String
  -- | UnboundVar String
  | RuntimeError String
  | DefaultError String
  deriving (Show, Eq)

data TypeError =
    TypeMismatch Type Type Expression
  | ParamsTypeMismatch [Type] [Type] Expression
  | CallNotProcVal Type
  | UnboundVar String
  | UnknownOperator String
  | TypeDefaultError String
  | UnimplInterface String Type
  | UnboundQualified String String  -- UnboundQualified module var
  | UnboundModule String
  | ModuleNameConflict String
  deriving (Eq)

instance Show TypeError where
  show (TypeMismatch t1 t2 expr) =
    concat [ "Expect type: ", show t1
           , ", but got: ", show t2
           , ", in expression: ", show expr
           ]
  show (ParamsTypeMismatch paramTypes argTypes proc) =
    concat [ "Expect types: ", show paramTypes
           , ", but got: ", show argTypes
           , ", when calling: ", show proc
           ]
  show (CallNotProcVal typ) =
    "Operator of call expression should be procedure but got: "
    `mappend` show typ
  show (UnboundVar name) =
    "Trying to check type of an unbound variable: " `mappend` name
  show (UnknownOperator name) =
    "Unknown operator: " `mappend` name
  show (UnimplInterface name typ) = concat
    [ "Try to offer a unsupplied interface ", name, " of type ", show typ ]
  show (UnboundQualified mName vName) = concat
    [ "Module ", mName, " does not export interface named ", vName ]
  show (UnboundModule m) = "Unknown module " `mappend` m
  show (ModuleNameConflict n) = "Module name conflict: " `mappend` n

type Unpacker a = ExpressedValue -> Try a

unpackNum :: Unpacker Integer
unpackNum (ExprNum n) = return n
unpackNum notNum      = error $ "Can't match type number to " `mappend` show notNum

unpackBool :: Unpacker Bool
unpackBool (ExprBool b) = return b
unpackBool notBool      =
  error $ "Can't match type boolean to " `mappend` show notBool

unpackProc :: Unpacker Procedure
unpackProc (ExprProc proc) = Right proc
unpackProc notProc         =
  error $ "Can't match type procedure to " `mappend` show notProc

data ModuleDef = ModuleDef String Interface ModuleBody deriving (Show, Eq)

newtype Interface = Interface [Declaration] deriving (Show, Eq)

newtype ModuleBody = ModuleBody [Definition] deriving (Show, Eq)

data Declaration = Declaration String Type deriving (Show, Eq)

data Definition = Definition String Expression deriving (Show, Eq)
