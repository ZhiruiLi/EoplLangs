module InferredLang.Data where

import qualified Data.List       as L
import qualified Data.Map        as M
import           Data.Maybe      (fromMaybe)
import qualified Text.Megaparsec as Mega

type Try = Either LangError

type TypeTry = Either TypeError

throwError :: LangError -> Try a
throwError = Left

type GeneralEnv = M.Map String

type Environment = GeneralEnv DenotedValue

type TypeEnvironment = GeneralEnv Type

empty :: GeneralEnv a
empty = M.empty

initEnvironment :: [(String, a)] -> GeneralEnv a
initEnvironment = M.fromList

extend :: String -> a -> GeneralEnv a -> GeneralEnv a
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

apply :: GeneralEnv a -> String -> Maybe a
apply = flip M.lookup

extendMany :: [(String, a)] -> GeneralEnv a -> GeneralEnv a
extendMany = flip (foldl func)
  where
    func env (var, val) = extend var val env

applyForce :: GeneralEnv a -> String -> a
applyForce env var = fromMaybe
  (error $ "Var " `mappend` var `mappend` " is not in environment!")
  (apply env var)

data Program = Prog Expression
  deriving (Show, Eq)

data Expression =
    ConstExpr ExpressedValue
  | VarExpr String
  | LetExpr [(String, Expression)] Expression
  | BinOpExpr BinOp Expression Expression
  | UnaryOpExpr UnaryOp Expression
  | CondExpr [(Expression, Expression)]
  | ProcExpr [(String, Maybe Type)] Expression
  | CallExpr Expression [Expression]
  | LetRecExpr [(Maybe Type, String, [(String, Maybe Type)], Expression)] Expression
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

type TypeVariable = Integer

data Type = TypeInt
          | TypeBool
          | TypeProc [Type] Type
          | TypeVar TypeVariable
          deriving (Eq)

instance Show Type where
  show TypeInt = "int"
  show TypeBool = "bool"
  show (TypeProc argTypes resType) =
    let argNames = fmap show argTypes
        sepComma = L.intersperse ", " argNames
        argStr = concat sepComma
    in  concat [ "(", "(", argStr, ")", " -> ", show resType, ")" ]
  show (TypeVar n) = "t" `mappend` show n

data TypeError =
    TypeMismatch Type Type Expression
  | ParamsTypeMismatch [Type] [Type] Expression
  | CallNotProcVal Type
  | UnboundVar String
  | UnknownOperator String
  | TypeUnifyError Type Type Expression
  | TypeOccurError Type Type Expression
  | TypeDefaultError String
  deriving (Eq)

instance Show TypeError where
  show (TypeMismatch t1 t2 expr) = concat
    [ "Expect type: ", show t1
    , ", but got: ", show t2
    , ", in expression: ", show expr
    ]
  show (ParamsTypeMismatch paramTypes argTypes proc) = concat
    [ "Expect types: ", show paramTypes
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
  show (TypeDefaultError msg) = msg
  show (TypeUnifyError t1 t2 expr) = concat
    [ "Fail to unify type ", show t1, " and type ", show t2, "." ]
  show (TypeOccurError t1 t2 expr) = concat
    [ "Type ", show t1, " occurs in type ", show t2 ]

type Substitution = [(TypeVariable, Type)]

emptySubst :: Substitution
emptySubst = []

applySubst :: Substitution -> Type -> Type
applySubst subst typ = case typ of
  TypeVar var -> fromMaybe typ (lookup var subst)
  TypeProc params res -> TypeProc params' res'
    where params' = fmap (applySubst subst) params
          res' = applySubst subst res
  _ -> typ

extendSubst :: TypeVariable -> Type -> Substitution -> Substitution
extendSubst var typ subst = (var, typ) : newSubst
  where newSubst = [(v, applySubst subst t) | (v, t) <- subst]

notOccurIn :: TypeVariable -> Type -> Bool
notOccurIn var typ = case typ of
  TypeVar var'      -> var /= var'
  TypeProc args res -> all (notOccurIn var) args && notOccurIn var res
  _                 -> True
