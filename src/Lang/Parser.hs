module Lang.Parser
( constExpr
, diffExpr
, isZeroExpr
, ifExpr
, varExpr
, letExpr
, expression
, minusExpr
, program
, parseProgram
) where

import           Control.Monad          (void)
import           Lang.Data.Expression
import           Lang.Data.Program
import           Text.Megaparsec
import           Text.Megaparsec.Expr
import qualified Text.Megaparsec.Lexer  as L
import           Text.Megaparsec.String

parseProgram :: String -> Either String Program
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

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

keyWord :: String -> Parser ()
keyWord w = string w *> notFollowedBy alphaNumChar *> spaceConsumer

reservedWords :: [String]
reservedWords  = ["let", "in", "if", "then", "else", "zero?", "minus"]

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

-- | ConstExpr ::= Number
constExpr :: Parser Expression
constExpr = ConstExpr <$> integer

-- | DiffExpr ::= -(Expression, Expression)
diffExpr :: Parser Expression
diffExpr = do
  _ <- minus
  pair <- body
  return $ uncurry DiffExpr pair
  where
    body = parens $ do
      expr1 <- expression
      _ <- comma
      expr2 <- expression
      return (expr1, expr2)

-- | IsZeroExpr ::= zero? (Expression)
isZeroExpr :: Parser Expression
isZeroExpr  = do
  _ <- keyWord "zero?"
  expr <- parens expression
  return $ IsZeroExpr expr

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

-- | MinusExpr ::= minus (Expression)
minusExpr :: Parser Expression
minusExpr = do
  _ <- keyWord "minus"
  expr <- parens expression
  return $ MinusExpr expr

-- | Expression ::= ConstExpr
--                | DiffExpr
--                | IsZeroExpr
--                | IfExpr
--                | VarExpr
--                | LetExpr
--                | MinusExpr
expression :: Parser Expression
expression = try constExpr
         <|> try diffExpr
         <|> try isZeroExpr
         <|> try ifExpr
         <|> try varExpr
         <|> try letExpr
         <|> minusExpr

program :: Parser Program
program = do
  _ <- spaceConsumer
  expr <- expression
  _ <- eof
  return $ Program expr
