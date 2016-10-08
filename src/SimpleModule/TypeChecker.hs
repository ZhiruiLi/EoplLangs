module SimpleModule.TypeChecker
( typeOf
, typeOfProgram
, typeOfExpression
) where

import           Control.Arrow        (second)
import           Control.Monad.Except
import           SimpleModule.Data

type TypeTry = Either TypeError

type TypeResult = TypeTry Type

typeOfProgram :: Program -> TypeResult
typeOfProgram (Prog mDefs body) = typeOfExpression mDefs body

typeOfExpression :: [ModuleDef] -> Expression -> TypeResult
typeOfExpression mDefs expr = addModuleDefs mDefs empty >>= typeOf expr

addModuleDefs :: [ModuleDef] -> TypeEnvironment -> TypeTry TypeEnvironment
addModuleDefs defs tenv = foldl func (return tenv) defs
  where func acc md = do { e <- acc; addModuleDef md e }

addModuleDef :: ModuleDef -> TypeEnvironment -> TypeTry TypeEnvironment
addModuleDef (ModuleDef name iface body) tenv = do
  binds <- matchInterface iface body tenv
  return $ extendNamed name binds tenv

matchInterface :: Interface -> ModuleBody -> TypeEnvironment
               -> TypeTry [(String, Type)]
matchInterface (Interface decls) (ModuleBody defs) tenv =
  matchInterface' decls
  where
    contain [] (Declaration s t) = throwError $ UnimplInterface s t
    contain (Definition s' e : defs) decl@(Declaration s t) =
      if s == s'
        then do { t' <- typeOf e tenv
                ; if t == t'
                    then return (s, t)
                    else throwError $ UnimplInterface s t
                }
        else defs `contain` decl
    func decl acc = do
      bind <- defs `contain` decl
      lst <- acc
      return (bind : lst)
    matchInterface' = foldr func (return [])

liftMaybe :: TypeError -> Maybe a -> TypeTry a
liftMaybe _ (Just x) = return x
liftMaybe y Nothing  = throwError y

liftEnvTry :: EnvTry a -> TypeTry a
liftEnvTry (Right x) = return x
liftEnvTry (Left (NamedEnvNotFound n)) = throwError $ UnboundVar n
liftEnvTry (Left (NamedEnvKeyNotFound e k)) = throwError $ QualifiedNotFound e k
liftEnvTry (Left (UnnamedEnvKeyNotFound e)) = throwError $ ModuleNotFound e

checkType :: Expression -> Type -> TypeEnvironment -> TypeResult
checkType expr expect tenv = do
  actual <- typeOf expr tenv
  if expect == actual
    then return expect
    else throwError (TypeMismatch expect actual expr)

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
typeOfVarExpr name tenv =  liftEnvTry $ apply tenv name
  where err = UnboundVar name

typeOfLetExpr :: [(String, Expression)] -> Expression -> TypeEnvironment
              -> TypeResult
typeOfLetExpr binds body tenv = do
  typeBinds <- reverse <$> foldl func (return []) binds
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
typeOfCondExpr [] tenv = throwError . TypeDefaultError $
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
typeOfExprs exprs tenv = reverse <$> foldl func (return []) exprs
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
    checkParamsType :: Type -> [Type] -> TypeResult
    checkParamsType (TypeProc paramTypes resType) argTypes =
      if paramTypes == argTypes
        then return resType
        else throwError $ ParamsTypeMismatch paramTypes argTypes proc
    checkParamsType wrongType _ = throwError $ CallNotProcVal wrongType

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

