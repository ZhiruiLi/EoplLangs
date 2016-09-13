module NamelessIntp.Parser
( expression
, program
, parseProgram
) where

import           Control.Monad          (void)
import           Data.Maybe             (fromMaybe)
import           NamelessIntp.Data
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

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

keyWord :: String -> Parser ()
keyWord w = string w *> notFollowedBy alphaNumChar *> spaceConsumer

reservedWords :: [String]
reservedWords  =
  [ "let", "in", "if", "then", "else", "zero?", "minus", "equal?", "greater?"
  , "less?", "proc", "cond", "end", "letrec"
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
constExpr = ConstExpr <$> signedInteger

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
  return $ IfExpr ifE thenE elseE

-- | VarExpr ::= Identifier
varExpr :: Parser Expression
varExpr = VarExpr <$> identifier

-- | LetExpr ::= let Identifier = Expression in Expression
letExpr :: Parser Expression
letExpr = do
  _ <- keyWord "let"
  var <- identifier
  _ <- equal
  val <- expression
  _ <- keyWord "in"
  body <- expression
  return $ LetExpr var val body

-- | ProcExpr ::= proc (Identifier) = Expression
procExpr :: Parser Expression
procExpr = do
  _ <- keyWord "proc"
  param <- parens identifier
  body <- expression
  return $ ProcExpr param body

-- | CallExpr ::= (Expression Expression)
callExpr :: Parser Expression
callExpr = parens $ do
  rator <- expression
  rand <- expression
  return $ CallExpr rator rand


-- | CondExpr ::= cond {Expression ==> Expression}* end
condExpr :: Parser Expression
condExpr = do
  _ <- keyWord "cond"
  pairs <- many $ try pair
  _ <- keyWord "end"
  return $ CondExpr pairs
  where
    pair = do e1 <- expression
              _ <- longArrow
              e2 <- expression
              return (e1, e2)

-- | LetRecExpr ::= letrec Identifier (Identifier) = Expression in Expression
letRecExpr :: Parser Expression
letRecExpr = do
  _ <- keyWord "letrec"
  procName <- identifier
  param <- parens identifier
  _ <- equal
  procBody <- expression
  _ <- keyWord "in"
  body <- expression
  return $ LetRecExpr procName param procBody body

-- | Expression ::= ConstExpr
--              ::= BinOpExpr
--              ::= UnaryOpExpr
--              ::= IfExpr
--              ::= VarExpr
--              ::= LetExpr
--              ::= ProcExpr
--              ::= CallExpr
--              ::= CondExpr
expression :: Parser Expression
expression = foldl1 (<|>) (fmap try expressionList)
  where
    expressionList =
      [ constExpr
      , binOpExpr
      , unaryOpExpr
      , ifExpr
      , varExpr
      , letExpr
      , procExpr
      , callExpr
      , condExpr
      , letRecExpr
      ]

program :: Parser Program
program = do
  _ <- spaceConsumer
  expr <- expression
  _ <- eof
  return $ Prog expr
