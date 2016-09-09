{-# LANGUAGE ExistentialQuantification #-}
module Repl
( repl
) where

import           Data.Char              (isSpace)
import           Data.Maybe             (fromMaybe)
import qualified ExplicitRefs.Evaluator as ExplicitRefs
import qualified ImplicitRefs.Evaluator as ImplicitRefs
import qualified LetLang.Evaluator      as LetLang
import qualified LetRecLang.Evaluator   as LetRecLang
import qualified NamelessIntp.Evaluator as NamelessIntp
import qualified ProcLang.Evaluator     as ProcLang
import           System.IO
import           Text.Megaparsec
import           Text.Megaparsec.String

data Lang = forall a b. (Show a) => Runnable (String -> Either String a)

defaultLangName = "ImplicitRefs"

lookupLang :: String -> Maybe Lang
lookupLang name = lookup name supportedLangs

defaultLang :: Lang
defaultLang = fromMaybe (error ("unknown lang: " `mappend` defaultLangName))
                        (lookupLang defaultLangName)

trim :: String -> String
trim = removeTailSpaces . removeHeadSpaces

removeHeadSpaces :: String -> String
removeHeadSpaces (x:xs) | isSpace x = removeHeadSpaces xs
removeHeadSpaces xs     = xs

removeTailSpaces :: String -> String
removeTailSpaces = reverse . removeHeadSpaces . reverse

supportedLangs :: [(String, Lang)]
supportedLangs =
  [ ("LetLang", Runnable LetLang.run)
  , ("ProcLang", Runnable ProcLang.run)
  , ("LetRecLang", Runnable LetRecLang.run)
  , ("Nameless", Runnable NamelessIntp.run)
  , ("ExplicitRefs", Runnable ExplicitRefs.run)
  , ("ImplicitRefs", Runnable ImplicitRefs.run)
  ]

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

flushStrLn :: String -> IO ()
flushStrLn str = putStrLn str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

data Command = NormalInput String
             | ChangeLang String
             | LoadFile String
             | Help
             | Unknown
             deriving (Show)

anyInput :: Parser Command
anyInput = many spaceChar >> tryAll
  where tryAll = try changeLang
             <|> try loadFile
             <|> try help
             <|> try normalInput

command :: String -> Parser String
command s = do
  _ <- string s >> skipSome spaceChar
  s <- many anyChar
  _ <- eof
  return (trim s)

changeLang :: Parser Command
changeLang = ChangeLang <$> command ":c"

loadFile :: Parser Command
loadFile = LoadFile <$> command ":l"

help :: Parser Command
help = do
  _ <- string ":h"
  _ <- many spaceChar
  _ <- eof
  return Help

normalInput :: Parser Command
normalInput = do
  c <- lookAhead anyChar
  NormalInput <$> if c == ':' then unexpected ":" else many anyChar

parseInput :: String -> Command
parseInput input = fromMaybe Unknown (parseMaybe anyInput input)

printHelp :: IO ()
printHelp = do
  flushStrLn ":h                  ==>  print help"
  flushStrLn ":c <language name>  ==>  change current language"
  flushStrLn ":l <file path>      ==>  load file"
  flushStrLn "-------------------------------------------------"
  flushStrLn "current supported languages: "
  print $ fmap fst supportedLangs

repl :: IO ()
repl = do
  putStrLn $ "Hello " `mappend` defaultLangName
  loop defaultLang
  where
    eval :: (Show a) => (String -> Either String a) -> String -> String
    eval evaluator input = case evaluator input of
      Left msg -> "error: " `mappend` msg
      Right x  -> show x
    loop :: Lang -> IO ()
    loop lang@(Runnable run) = do
      str <- readPrompt "> "
      case parseInput str of
        Help    ->
          do printHelp
             loop lang
        Unknown ->
          do flushStrLn "Unknown command !"
             printHelp
             loop lang
        ChangeLang name ->
          case lookupLang name of
            Nothing ->
              do flushStrLn "Unsupport language !"
                 loop lang
            Just newLang ->
              do flushStrLn $ "Change current language to: " `mappend` name
                 loop newLang
        LoadFile path ->
          do code <- readFile path
             flushStrLn $ eval run code
             loop lang
        NormalInput code ->
          do flushStrLn $ eval run code
             loop lang
