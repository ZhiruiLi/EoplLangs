module InferredLang.TypeChecker
( typeOf
, typeOfProgram
, typeOfExpression
, checkProgramType
, checkExpressionType
) where

import           Control.Arrow            (second)
import           Control.Monad.Except
import           Control.Monad.State.Lazy (StateT, get, put, runStateT, state)
import           InferredLang.Data        hiding (throwError)
import           InferredLang.Parser      (parseProgram)

type TypeStateTry = StateT (TypeVariable, Substitution) TypeTry

type TypeResult = TypeStateTry Type

printType :: String -> IO ()
printType input = case parseProgram input of
  Right prog -> case typeOfProgram prog of
                  Right typ -> print typ
                  Left msg  -> print msg
  Left msg -> print msg

checkProgramType :: Program -> Type -> TypeTry ()
checkProgramType (Prog expr) = checkExpressionType expr

checkExpressionType :: Expression -> Type -> TypeTry ()
checkExpressionType expr typ =
  fst <$> runStateT (checkType expr typ empty) (0, emptySubst)

typeOfProgram :: Program -> TypeTry Type
typeOfProgram (Prog expr) = typeOfExpression expr

typeOfExpression :: Expression -> TypeTry Type
typeOfExpression expr = fst <$> runStateT (typeOf expr empty) (0, emptySubst)

liftMaybe :: TypeError -> Maybe a -> TypeStateTry a
liftMaybe _ (Just x) = return x
liftMaybe y Nothing  = throwError y

liftTry :: TypeTry a -> TypeStateTry a
liftTry (Left err)  = throwError err
liftTry (Right val) = return val

checkType :: Expression -> Type -> TypeEnvironment -> TypeStateTry ()
checkType expr expect tenv = do
  actual <- typeOf expr tenv
  unifyTypes expect actual expr


unifyTypes :: Type -> Type -> Expression -> TypeStateTry ()
unifyTypes typ1 typ2 expr = do
  subst <- getSubst
  let t1 = applySubst subst typ1
  let t2 = applySubst subst typ2
  unifyTypes' t1 t2
  where
    unifyTypes' :: Type -> Type -> TypeStateTry ()
    unifyTypes' t1 t2 | t1 == t2 = return ()
    unifyTypes' t1@(TypeVar var) t2
      | var `notOccurIn` t2 = updateSubst $ extendSubst var t2
      | otherwise = throwError (TypeOccurError t1 t2 expr)
    unifyTypes' t1 t2@(TypeVar var)
      | var `notOccurIn` t1 = updateSubst $ extendSubst var t1
      | otherwise = throwError (TypeOccurError t2 t1 expr)
    unifyTypes' t1@(TypeProc params1 res1)
                t2@(TypeProc params2 res2) = do
      paramPairs <- safeZip params1 params2
      unifyAll $ mappend paramPairs [(res1, res2)]
      where
        safeZip :: [a] -> [b] -> TypeStateTry [(a, b)]
        safeZip p1 p2 = if length p1 == length p2
          then return $ zip p1 p2
          else throwError $ TypeUnifyError t1 t2 expr
    unifyTypes' t1 t2 = throwError $ TypeUnifyError t1 t2 expr
    unifyAll :: [(Type, Type)] -> TypeStateTry ()
    unifyAll = foldl (\acc (t1, t2) -> acc >> unifyTypes' t1 t2) (return ())

nextVar :: TypeStateTry TypeVariable
nextVar = do
  (var, subst) <- get
  put (succTypeVar var, subst)
  return var

nextVarType :: TypeStateTry Type
nextVarType = TypeVar <$> nextVar

getSubst :: TypeStateTry Substitution
getSubst = snd <$> get

setSubst :: Substitution -> TypeStateTry ()
setSubst subst = do (var, _) <- get
                    put (var, subst)

updateSubst :: (Substitution -> Substitution) -> TypeStateTry ()
updateSubst func = do s <- getSubst
                      setSubst $ func s

ensureType :: Maybe Type -> TypeStateTry Type
ensureType Nothing    = nextVarType
ensureType (Just typ) = return typ

typeOf :: Expression -> TypeEnvironment -> TypeResult
typeOf (ConstExpr val) _            = typeOfConstExpr val
typeOf (VarExpr name) tenv          = typeOfVarExpr name tenv
typeOf (LetExpr binds body) tenv    = typeOfLetExpr binds body tenv
typeOf (BinOpExpr op e1 e2) tenv    = typeOfBinOpExpr op e1 e2 tenv
typeOf (UnaryOpExpr op e) tenv      = typeOfUnaryOpExpr op e tenv
typeOf (IfExpr e1 e2 e3) tenv       = typeOfIfExpr e1 e2 e3 tenv
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
  typeBinds <- foldr func (return []) binds
  typeOf body (extendMany typeBinds tenv)
  where
    func (name, expr) acc = do
      typ <- typeOf expr tenv
      typeBinds <- acc
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

unifyCheckType :: Expression -> Type -> TypeEnvironment -> TypeStateTry ()
unifyCheckType expr typ tenv = do
  t <- typeOf expr tenv
  unifyTypes t typ expr

typeOfBinOpExpr :: BinOp -> Expression -> Expression -> TypeEnvironment
                -> TypeResult
typeOfBinOpExpr op e1 e2 tenv = do
  types <- liftMaybe (UnknownOperator (show op)) (lookup op typedBinOps)
  let (t1, t2, tres) = types
  unifyCheckType e1 t1 tenv
  unifyCheckType e2 t2 tenv
  return tres

typeOfUnaryOpExpr :: UnaryOp -> Expression -> TypeEnvironment -> TypeResult
typeOfUnaryOpExpr op e tenv = do
  types <- liftMaybe (UnknownOperator (show op)) (lookup op typedUnaryOps)
  let (t, tres) = types
  unifyCheckType e t tenv
  return tres

typeOfIfExpr :: Expression -> Expression -> Expression -> TypeEnvironment
             -> TypeResult
typeOfIfExpr ifE thenE elseE tenv = do
  unifyCheckType ifE TypeBool tenv
  thenT <- typeOf thenE tenv
  elseT <- typeOf elseE tenv
  unifyTypes thenT elseT (IfExpr ifE thenE elseE)
  return thenT

ensureAllBinds :: [(a, Maybe Type)] -> TypeStateTry [(a, Type)]
ensureAllBinds = foldr func (return [])
  where func (name, mayT) acc = do
          t <- ensureType mayT
          ((name, t) :) <$> acc

typeOfProcExpr :: [(String, Maybe Type)] -> Expression -> TypeEnvironment
               -> TypeResult
typeOfProcExpr mayParams body tenv = do
  params <- ensureAllBinds mayParams
  resType <- typeOf body (extendMany params tenv)
  let paramTypes = fmap snd params
  return $ TypeProc paramTypes resType

typeOfExprs :: [Expression] -> TypeEnvironment -> TypeStateTry [Type]
typeOfExprs exprs tenv = foldr func (return []) exprs
  where
    func expr acc = do
      typ <- typeOf expr tenv
      types <- acc
      return $ typ : types

typeOfCallExpr :: Expression -> [Expression] -> TypeEnvironment -> TypeResult
typeOfCallExpr ratorE argEs tenv = do
  resT <- nextVarType
  procT <- typeOf ratorE tenv
  argTs <- typeOfExprs argEs tenv
  unifyTypes procT (TypeProc argTs resT) (CallExpr ratorE argEs)
  let (TypeProc _ resT') = procT
  (`applySubst` resT') <$> getSubst

typeOfLetRecExpr :: [(Maybe Type, String, [(String, Maybe Type)], Expression)]
                 -> Expression -> TypeEnvironment
                 -> TypeResult
typeOfLetRecExpr binds body tenv = undefined

-- typeOfLetRecExpr binds body tenv = do
  -- checkAllBinds binds
  -- typeOf body bodyEnv
  -- where
    -- getRecBinds :: [(Maybe Type, String, [(String, Maybe Type)]
                -- -> TypeStateTry [(Type, String, [(String, Type)]
    -- getRecBinds [] = return []
    -- getRecBinds ((mayResT, name, params, expr) : remain) = do
      -- resT <- ensureType mayResT
      -- let mayTs = fmap snd params
      -- paramTs <- ensureAllTypes mayTs
      -- let params' = zip (fmap fst params) paramTs
      -- ((resT, name, params') :) <$> getRecBinds remain
    -- getBodyEnv :: TypeStateTry TypeEnvironment
    -- getBodyEnv = do
      -- binds' <- getBodyEnv binds
      -- return $ extendMany binds' tenv
    -- checkAllBinds [] = return ()
    -- checkAllBinds ((res, name, params, body) : remain) =
      -- checkType body res (extendMany params bodyEnv) >> checkAllBinds remain
--
