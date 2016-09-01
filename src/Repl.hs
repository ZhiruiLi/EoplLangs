{-# LANGUAGE ExistentialQuantification #-}
module Repl
( repl
) where

import           Data.Maybe        (fromMaybe)
import qualified LetLang.Evaluator as Let
import           System.IO

data Runnable = forall a b. (Show a) => Runnable (String -> Either String a)

defaultLang = "LetLang"

supportedLangs :: [(String, Runnable)]
supportedLangs = fmap
  (\p -> (fst p, Runnable (snd p)))
  [ ("LetLang", Let.run)
  ]

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

repl :: IO ()
repl = do
  print $ "Hello " `mappend` defaultLang
  loop (getLang defaultLang)
  where
    getLang lang = fromMaybe (error ("unknown lang: " `mappend` lang)) (lookup lang supportedLangs)
    eval :: (Show a) => (String -> Either String a) -> String -> String
    eval evaluator input = case evaluator input of
      Left msg -> "error: " `mappend` msg
      Right x  -> show x
    loop r@(Runnable run) = do
      str <- readPrompt "> "
      putStrLn $ eval run str
      loop r





