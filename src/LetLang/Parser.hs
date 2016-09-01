module LetLang.Parser
( constExpr
, binOpExpr
, isZeroExpr
, ifExpr
, varExpr
, letExpr
, expression
, minusExpr
, program
, parseProgram
) where

import           Control.Monad           (void)
import           Data.Maybe              (fromMaybe)
import           LetLang.Data.Expression
import           LetLang.Data.Program
import           Text.Megaparsec
import           Text.Megaparsec.Expr
import qualified Text.Megaparsec.Lexer   as L
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
reservedWords  =
  [ "let", "in", "if", "then", "else", "zero?", "minus", "equal?"
  , "greater?", "less?" ]

binOpsMap :: [(String, Op)]
binOpsMap =
  [ ("+", Add), ("-", Sub), ("*", Mul), ("/", Div), ("equal?", Eq)
  , ("greater?", Gt), ("less?", Le) ]

binOp :: Parser Op
binOp = do
  opStr <- foldl1 (<|>) (fmap (try . symbol . fst) binOpsMap)
  return $ fromMaybe
    (error ("Unknown operator '" `mappend` opStr `mappend` "'"))
    (lookup opStr binOpsMap)

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

-- | BinOpExpr ::= BinOp(Expression, Expression)
binOpExpr :: Parser Expression
binOpExpr = do
  op <- binOp
  exprPair <- parens body
  return $ uncurry (BinOpExpr op) exprPair
  where
    body = do
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
--                | BinOpExpr
--                | IsZeroExpr
--                | IfExpr
--                | VarExpr
--                | LetExpr
--                | MinusExpr
expression :: Parser Expression
expression = try constExpr
         <|> try binOpExpr
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
