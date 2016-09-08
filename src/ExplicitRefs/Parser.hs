module ExplicitRefs.Parser
( expression
, program
, parseProgram
) where

import           Control.Monad          (void)
import           Data.Maybe             (fromMaybe)
import           ExplicitRefs.Data
import           Text.Megaparsec
import           Text.Megaparsec.Expr
import qualified Text.Megaparsec.Lexer  as L
import           Text.Megaparsec.String

parseProgram :: String -> Try Program
parseProgram input = case runParser program "Program Parser" input of
  Left err -> Left $ show err
  Right p  -> Right p

spaceConsumer :: Parser ()
spaceConsumer = L.space (void spaceChar) lineCmnt blockCmnt
  where lineCmnt  = L.skipLineComment "//"
        blockCmnt = L.skipBlockComment "/*" "*/"

symbol = L.symbol spaceConsumer

parens = between (symbol "(") (symbol ")")
minus = symbol "-"
equal = symbol "="
comma = symbol ","
longArrow = symbol "==>"
semiColon = symbol ";"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

keyWord :: String -> Parser ()
keyWord w = string w *> notFollowedBy alphaNumChar *> spaceConsumer

reservedWords :: [String]
reservedWords  =
  [ "let*", "let", "in", "if", "then", "else", "zero?", "minus"
  , "equal?", "greater?", "less?", "cond", "end", "proc", "letrec"
  , "begin", "newref", "deref", "setref", "list"
  ]

binOpsMap :: [(String, BinOp)]
binOpsMap =
  [ ("+", Add), ("-", Sub), ("*", Mul), ("/", Div), ("equal?", Eq)
  , ("greater?", Gt), ("less?", Le) ]

binOp :: Parser BinOp
binOp = do
  opStr <- foldl1 (<|>) (fmap (try . symbol . fst) binOpsMap)
  return $ fromMaybe
    (error ("Unknown operator '" `mappend` opStr `mappend` "'"))
    (lookup opStr binOpsMap)

unaryOpsMap :: [(String, UnaryOp)]
unaryOpsMap =
  [ ("minus", Minus), ("zero?", IsZero) ]

unaryOp :: Parser UnaryOp
unaryOp = do
  opStr <- foldl1 (<|>) (fmap (try . symbol . fst) unaryOpsMap)
  return $ fromMaybe
    (error ("Unknown operator '" `mappend` opStr `mappend` "'"))
    (lookup opStr unaryOpsMap)

-- | Identifier ::= String (without reserved words)
identifier :: Parser String
identifier = lexeme (p >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` reservedWords
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

integer :: Parser Integer
integer = lexeme L.integer

signedInteger :: Parser Integer
signedInteger = L.signed spaceConsumer integer

-- expressionPair ::= (Expression, Expression)
expressionPair :: Parser (Expression, Expression)
expressionPair = parens $ do
  expr1 <- expression
  _ <- comma
  expr2 <- expression
  return (expr1, expr2)

-- | ConstExpr ::= Number
constExpr :: Parser Expression
constExpr = ConstExpr . ExprNum <$> signedInteger

-- | BinOpExpr ::= BinOp (Expression, Expression)
binOpExpr :: Parser Expression
binOpExpr = do
  op <- binOp
  exprPair <- expressionPair
  return $ uncurry (BinOpExpr op) exprPair

-- | UnaryOpExpr ::= UnaryOp (Expression)
unaryOpExpr :: Parser Expression
unaryOpExpr = do
  op <- unaryOp
  expr <- parens expression
  return $ UnaryOpExpr op expr

-- | IfExpr ::= if Expression then Expression
ifExpr :: Parser Expression
ifExpr = do
  _ <- keyWord "if"
  ifE <- expression
  _ <- keyWord "then"
  thenE <- expression
  _ <- keyWord "else"
  elseE <- expression
  return $ CondExpr [(ifE, thenE), (ConstExpr (ExprBool True), elseE)]

-- | VarExpr ::= Identifier
varExpr :: Parser Expression
varExpr = VarExpr <$> identifier

-- | LetExpr ::= let {Identifier = Expression}* in Expression
letExpr :: Parser Expression
letExpr = letFamilyExpr "let" LetExpr

letFamilyExpr :: String
              ->  ([(String, Expression)] -> Expression -> Expression)
              -> Parser Expression
letFamilyExpr letType builder = do
  _ <- keyWord letType
  bindings <- many binding
  _ <- keyWord "in"
  body <- expression
  return $ builder bindings body
  where
    binding = try $ do
      var <- identifier
      _ <- equal
      val <- expression
      return (var, val)

-- | LetrecExpr ::= letrec {Identifier (Identifier) = Expression} in Expression
letRecExpr :: Parser Expression
letRecExpr = do
  _ <- keyWord "letrec"
  procBindings <- many procBinding
  _ <- keyWord "in"
  recBody <- expression
  return $ LetRecExpr procBindings recBody
  where
    procBinding = try $ do
      procName <- identifier
      param <- parens identifier
      _ <- equal
      procBody <- expression
      return (procName, param, procBody)


-- | ManyExprs ::= <empty>
--             ::= Many1Exprs
manyExprs :: Parser [Expression]
manyExprs = sepBy expression comma

-- | Many1Exprs ::= Expression
--              ::= Expression , Many1Exprs
many1Exprs :: Parser [Expression]
many1Exprs = sepBy1 expression comma

-- | CondExpr ::= cond {Expression ==> Expression}* end
condExpr :: Parser Expression
condExpr = do
  _ <- keyWord "cond"
  pairs <- many pair
  _ <- keyWord "end"
  return $ CondExpr pairs
  where
    pair = try $ do
      expr1 <- expression
      _ <- longArrow
      expr2 <- expression
      return (expr1, expr2)

-- | ProcExpr ::= proc ({Identifier}*) Expression
procExpr :: Parser Expression
procExpr = do
  _ <- keyWord "proc"
  param <- parens identifier
  body <- expression
  return $ ProcExpr param body

-- | CallExpr ::= (Expression {Expression}*)
callExpr :: Parser Expression
callExpr = parens $ do
  rator <- expression
  rand <- expression
  return $ CallExpr rator rand

-- | BeginExpr ::= begin BeginBody end
--
--   BeginBody ::= <empty>
--             ::= Expression BeginBodyTail
--
--   BeginBodyTail ::= <empty>
--                 ::= ; Expression BeginBodyTail
beginExpr :: Parser Expression
beginExpr = do
  _ <- keyWord "begin"
  exprs <- sepBy1 (try expression) semiColon
  _ <- keyWord "end"
  return $ BeginExpr exprs

-- | NewRefExpr ::= newref(Expression)
newRefExpr :: Parser Expression
newRefExpr = do
  _ <- keyWord "newref"
  expr <- parens expression
  return $ NewRefExpr expr

-- | DeRefExpr ::= deref(Expression)
deRefExpr :: Parser Expression
deRefExpr = do
  _ <- keyWord "deref"
  expr <- parens expression
  return $ DeRefExpr expr

-- | SetRefExpr ::= setref (Expression, Expression)
setRefExpr :: Parser Expression
setRefExpr = do
  _ <- keyWord "setref"
  pair <- parens body
  return $ uncurry SetRefExpr pair
  where
    body = do
      e1 <- expression
      _ <- comma
      e2 <- expression
      return (e1, e2)

-- | ListExpr ::= list(ListBody)
--   ListBody ::= <empty>
--            ::= Expression ListBodyTail
--   ListBodyTail ::= <empty>
--                ::= ,Expression ListBodyTail
listExpr :: Parser Expression
listExpr = do
  _ <- keyWord "list"
  body <- parens $ sepBy expression comma
  return $ ListExpr body

-- | Expression ::= ConstExpr
--              ::= BinOpExpr
--              ::= UnaryOpExpr
--              ::= IfExpr
--              ::= CondExpr
--              ::= VarExpr
--              ::= LetExpr
--              ::= ProcExpr
--              ::= CallExpr
--              ::= BeginExpr
--              ::= NewRefExpr
--              ::= DeRefExpr
--              ::= SetRefexpr
expression :: Parser Expression
expression = try constExpr
         <|> try binOpExpr
         <|> try unaryOpExpr
         <|> try ifExpr
         <|> try condExpr
         <|> try varExpr
         <|> try letExpr
         <|> try procExpr
         <|> try callExpr
         <|> try letRecExpr
         <|> try beginExpr
         <|> try newRefExpr
         <|> try deRefExpr
         <|> try setRefExpr
         <|> try listExpr

program :: Parser Program
program = do
  _ <- spaceConsumer
  expr <- expression
  _ <- eof
  return $ Prog expr
