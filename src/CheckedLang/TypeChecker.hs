module CheckedLang.TypeChecker
( typeOf
, typeOfProgram
, typeOfExpression
, checkProgramType
, checkExpressionType
) where

import           CheckedLang.Data
import           Control.Arrow    (second)

type TypeTry = Either TypeError

type TypeResult = TypeTry Type

checkProgramType :: Program -> Type -> TypeResult
checkProgramType (Prog expr) = checkExpressionType expr

checkExpressionType :: Expression -> Type -> TypeResult
checkExpressionType expr typ = checkType expr typ empty

typeOfProgram :: Program -> TypeResult
typeOfProgram (Prog expr) = typeOfExpression expr

typeOfExpression :: Expression -> TypeResult
typeOfExpression expr = typeOf expr empty

liftMaybe :: TypeError -> Maybe a -> TypeTry a
liftMaybe _ (Just x) = return x
liftMaybe y Nothing  = throwTypeError y

throwTypeError :: TypeError -> TypeTry a
throwTypeError = Left

checkType :: Expression -> Type -> TypeEnvironment -> TypeResult
checkType expr expect tenv = do
  actual <- typeOf expr tenv
  if expect == actual
    then return expect
    else throwTypeError (TypeMismatch expect actual expr)

typeOf :: Expression -> TypeEnvironment -> TypeResult
typeOf (ConstExpr val) _            = typeOfConstExpr val
typeOf (VarExpr name) tenv          = typeOfVarExpr name tenv
typeOf (LetExpr binds body) tenv    = typeOfLetExpr binds body tenv
typeOf (BinOpExpr op e1 e2) tenv    = typeOfBinOpExpr op e1 e2 tenv
typeOf (UnaryOpExpr op e) tenv      = typeOfUnaryOpExpr op e tenv
typeOf (CondExpr branches) tenv     = typeOfCondExpr branches tenv
typeOf (ProcExpr params body) tenv  = typeOfProcExpr params body tenv
typeOf (CallExpr proc args) tenv    = typeOfCallExpr proc args tenv
typeOf (LetRecExpr binds body) tenv = typeOfLetRecExpr binds body tenv

typeOfConstExpr :: ExpressedValue -> TypeResult
typeOfConstExpr (ExprNum n)  = return TypeInt
typeOfConstExpr (ExprBool b) = return TypeBool

typeOfVarExpr :: String -> TypeEnvironment -> TypeResult
typeOfVarExpr name tenv = liftMaybe err $ apply tenv name
  where err = UnboundVar name

typeOfLetExpr :: [(String, Expression)] -> Expression -> TypeEnvironment
              -> TypeResult
typeOfLetExpr binds body tenv = do
  typeBinds <- foldl func (return []) binds
  typeOf body (extendMany typeBinds tenv)
  where
    func acc (name, expr) = do
      typeBinds <- acc
      typ <- typeOf expr tenv
      return $ (name, typ) : typeBinds

typedBinOps :: [(BinOp, (Type, Type, Type))]
typedBinOps = concat
  [ attachTypes binBoolOps TypeBool TypeBool TypeBool
  , attachTypes binNumToNumOps TypeInt TypeInt TypeInt
  , attachTypes binNumToBoolOps TypeInt TypeInt TypeBool
  ]
  where
    attachTypes ops t1 t2 tres = fmap (\op -> (op, (t1, t2, tres))) ops
    binBoolOps = []
    binNumToNumOps = [ Add, Sub, Mul, Div ]
    binNumToBoolOps = [ Gt, Le, Eq ]

typedUnaryOps :: [(UnaryOp, (Type, Type))]
typedUnaryOps = concat
  [ attachTypes unaryBoolOps TypeBool TypeBool
  , attachTypes unaryNumToNumOps TypeInt TypeInt
  , attachTypes unaryNumToBoolOps TypeInt TypeBool
  ]
  where
    attachTypes ops t tres = fmap (\op -> (op, (t, tres))) ops
    unaryBoolOps = []
    unaryNumToNumOps = [ Minus ]
    unaryNumToBoolOps = [ IsZero ]

typeOfBinOpExpr :: BinOp -> Expression -> Expression -> TypeEnvironment
                -> TypeResult
typeOfBinOpExpr op e1 e2 tenv = do
  types <- liftMaybe (UnknownOperator (show op)) (lookup op typedBinOps)
  let (t1, t2, tres) = types
  checkType e1 t1 tenv
  checkType e2 t2 tenv
  return tres

typeOfUnaryOpExpr :: UnaryOp -> Expression -> TypeEnvironment -> TypeResult
typeOfUnaryOpExpr op e tenv = do
  types <- liftMaybe (UnknownOperator (show op)) (lookup op typedUnaryOps)
  let (t, tres) = types
  checkType e t tenv
  return tres

typeOfCondExpr :: [(Expression, Expression)] -> TypeEnvironment -> TypeResult
typeOfCondExpr [] tenv = throwTypeError . TypeDefaultError $
  "Condition expression should contain at least one sub-expressions."
typeOfCondExpr ((cond, expr) : remain) tenv = do
  checkType cond TypeBool tenv
  typ <- typeOf expr tenv
  checkRemainTypes remain typ
  where
    checkRemainTypes [] typ = return typ
    checkRemainTypes ((cond, expr) : remain) typ = do
      checkType cond TypeBool tenv
      checkType expr typ tenv
      checkRemainTypes remain typ

typeOfProcExpr :: [(String, Type)] -> Expression -> TypeEnvironment
               -> TypeResult
typeOfProcExpr params body tenv = do
  resType <- typeOf body (extendMany params tenv)
  let paramTypes = fmap snd params
  return $ TypeProc paramTypes resType

typeOfExprs :: [Expression] -> TypeEnvironment -> TypeTry [Type]
typeOfExprs exprs tenv = foldl func (return []) exprs
  where
    func acc expr = do
      types <- acc
      typ <- typeOf expr tenv
      return $ typ : types

typeOfCallExpr :: Expression -> [Expression] -> TypeEnvironment -> TypeResult
typeOfCallExpr proc args tenv = do
  procType <- typeOf proc tenv
  argTypes <- typeOfExprs args tenv
  checkParamsType procType argTypes
  where
    checkParamsType (TypeProc paramTypes resType) argTypes =
      if paramTypes == argTypes
        then return resType
        else throwTypeError $ ParamsTypeMismatch paramTypes argTypes proc
    checkParamsType wrongType _ = throwTypeError $ CallNotProcVal wrongType

typeOfLetRecExpr :: [(Type, String, [(String, Type)], Expression)]
                 -> Expression -> TypeEnvironment
                 -> TypeResult
typeOfLetRecExpr binds body tenv = checkAllBinds binds >> typeOf body bodyEnv
  where
    recBinds = [ (name, TypeProc (fmap snd paramTs) resT)
               | (resT, name, paramTs, _) <- binds ]
    bodyEnv = extendMany recBinds tenv
    checkAllBinds [] = return ()
    checkAllBinds ((res, name, params, body) : remain) =
      checkType body res (extendMany params bodyEnv) >> checkAllBinds remain

