module LetLang.EvaluatorSuite
( tests
) where

import           Data.List                   (stripPrefix)
import           LetLang.Data.Environment
import           LetLang.Data.ExpressedValue
import           LetLang.Evaluator
import           LetLang.Parser              (expression)
import           Test.HUnit
import           Text.Megaparsec

tests :: Test
tests = TestList
  [ testEq "Eval const" (NumVal 3) "3"
  , testEq "Eval bounded var" (NumVal 5) "v"
  , testNoBound "Eval no-bounded var" "y"
  , testEq "Eval binary num-to-num operator" (NumVal 5) "-(10, 5)"
  , testEq "Eval binary num-to-bool operator (true case)" 
           (BoolVal True) "greater?(4, 3)"
  , testEq "Eval binary num-to-bool operator (false case)" 
           (BoolVal False) "less?(5,2)"
  , testEq "Eval let"
           (NumVal 3)
           $ unlines
              [ "let z = 5"
              , "in let x = 3"
              , "   in let y = - (x, 1)"
              , "      in let x = 4"
              , "         in -(z, -(x,y))"
              ]
  , testEq "Eval true branch of if expression"
           (NumVal 3)
           "if zero? (0) then 3 else 4"
  , testEq "Eval false branch of if expression"
           (NumVal 4)
           "if zero? (5) then 3 else 4"
  , testEq "Eval minus" (NumVal (-1)) "minus(1)"
  ]

initEnv :: Environment
initEnv = initEnvironment [("i", NumVal 1), ("v", NumVal 5), ("x", NumVal 10)]

testEq :: String -> ExpressedValue -> String -> Test
testEq msg expect input = TestCase $
  assertEqual msg (Right expect) evalRes
  where
    evalRes = case runParser expression "Test equal case" input of
      Right pRes  -> valueOf pRes initEnv
      Left pError -> Left $ show pError

testNoBound :: String -> String -> Test
testNoBound msg input = TestCase $
  assertEqual msg noBound True
  where
    noBound = case runParser expression "Test nobound case" input of
      Right pRes -> case valueOf pRes initEnv of
                      Left s -> case stripPrefix "Not in scope: " s of
                                  Nothing -> False
                                  _       -> True
                      _ -> False
      _ -> False
