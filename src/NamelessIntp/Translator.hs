module NamelessIntp.Translator
( translateProgram
, translate
) where

import           NamelessIntp.Data

type TranslateResult = Try NamelessExpression

translateProgram :: Program -> Either String NamelessProgram
translateProgram (Prog expr) = case translate expr emptySEnv of
  Left msg   -> Left msg
  Right expr -> Right $ NamelessProg expr

translate :: Expression -> StaticEnvironment -> TranslateResult
translate (ConstExpr i) _                 = transConstExpr i
translate (VarExpr name) senv             = transVarExpr name senv
translate (LetExpr name expr body) senv   = transLetExpr name expr body senv
translate (BinOpExpr op expr1 expr2) senv = transBinOpExpr op expr1 expr2 senv
translate (UnaryOpExpr op expr) senv      = transUnaryOpExpr op expr senv
translate (IfExpr ifE thenE elseE) senv   = transIfExpr ifE thenE elseE senv
translate (ProcExpr param body) senv      = transProcExpr param body senv
translate (CallExpr rator rand) senv      = transCallExpr rator rand senv

transConstExpr :: Integer -> TranslateResult
transConstExpr i = Right $ NamelessConstExpr i

transVarExpr :: String -> StaticEnvironment -> TranslateResult
transVarExpr name senv = case applySEnvSafe senv name of
  Nothing -> Left $ "Not in scope: " `mappend` name
  Just i  -> Right $ NamelessVarExpr i

transLetExpr :: String -> Expression -> Expression -> StaticEnvironment
             -> TranslateResult
transLetExpr name expr body senv = do
  e <- translate expr senv
  b <- translate body newSEnv
  return $ NamelessLetExpr e b
  where newSEnv = extendSEnv name senv

transBinOpExpr :: BinOp -> Expression -> Expression -> StaticEnvironment
               -> TranslateResult
transBinOpExpr op expr1 expr2 senv = do
  e1 <- translate expr1 senv
  e2 <- translate expr2 senv
  return $ NamelessBinOpExpr op e1 e2

transUnaryOpExpr :: UnaryOp -> Expression -> StaticEnvironment
                 -> TranslateResult
transUnaryOpExpr op expr senv = do
  e <- translate expr senv
  return $ NamelessUnaryOpExpr op e

transIfExpr :: Expression -> Expression -> Expression -> StaticEnvironment
            -> TranslateResult
transIfExpr ifE thenE elseE senv = do
  i <- translate ifE senv
  t <- translate thenE senv
  e <- translate elseE senv
  return $ NamelessIfExpr i t e

transProcExpr :: String -> Expression -> StaticEnvironment
              -> TranslateResult
transProcExpr param body senv = do
  b <- translate body (extendSEnv param senv)
  return $ NamelessProcExpr b

transCallExpr :: Expression -> Expression -> StaticEnvironment
              -> TranslateResult
transCallExpr rator rand senv = do
  r <- translate rator senv
  d <- translate rand senv
  return $ NamelessCallExpr r d
