module ContinuationPassing.EvaluatorSuite
( tests
) where

import           ContinuationPassing.Data
import           ContinuationPassing.Evaluator
import           ContinuationPassing.Parser    (expression)
import           Data.List                     (stripPrefix)
import           Test.HUnit
import           Text.Megaparsec

tests :: Test
tests = TestList
  [ testEq "Eval const" (ExprNum 3) "3"
  , testEq "Eval bounded var" (ExprNum 5) "v"
  , testNoBound "Eval no-bounded var" "y"
  , testEq "Eval binary num-to-num operator" (ExprNum 5) "-(10, 5)"
  , testEq "Eval binary num-to-bool operator (true case)"
           (ExprBool True) "greater?(4, 3)"
  , testEq "Eval binary num-to-bool operator (false case)"
           (ExprBool False) "less?(5,2)"
  , testEq "Eval minus" (ExprNum (-1)) "minus(1)"
  , testLet
  , testCond
  , testProc
  ]

testLet :: Test
testLet = TestList
  [ testEq "Eval let"
           (ExprNum 1)
           $ unlines
             [ "let x = 30"
             , "in let x = -(x,1)"
             , "       y = -(x,2)"
             , "   in -(x,y)"
             ]
  , testEq "Eval multi let items"
           (ExprNum 3)
           $ unlines
             [ "let x = 0"
             , "    y = 1"
             , "    z = 2"
             , "in +(x, +(y, z))"
             ]
  , testEq "Eval letrec"
           (ExprNum 12)
           $ unlines
             [ "letrec double(x)"
             , "        = if zero?(x) then 0 else -((double -(x,1)), -2)"
             , "in (double 6)"
             ]
  , testEq "Eval co-recursion"
           (ExprNum 1)
           $ unlines
             [ "letrec"
             , "  even(x) = if zero?(x) then 1 else (odd -(x,1))"
             , "  odd(x) = if zero?(x) then 0 else (even -(x,1))"
             , "in (odd 13)"
             ]
  ]

testCond :: Test
testCond = TestList
  [ testEq "Eval true branch of if expression"
           (ExprNum 3)
           "if zero? (0) then 3 else 4"
  , testEq "Eval false branch of if expression"
           (ExprNum 4)
           "if zero? (5) then 3 else 4"
  , testEq "Eval nested if expression"
           (ExprNum 2)
           "if zero? (1) then 1 else if zero? (0) then 2 else 3"
  ]

testProc :: Test
testProc = TestList
  [ testEq "Eval proc and call expression (no parameter)"
           (ExprNum 1)
           "(proc () 1)"
  , testEq "Eval proc and call expression (1 parameter)"
           (ExprNum 2)
           "(proc (x) + (x, x) 1)"
  , testEq "Eval proc and call expression (many parameters)"
           (ExprNum 7)
           "(proc (x y z) + (x, * (y, z)) 1 2 3)"
  , testEq "Eval named proc"
           (ExprNum 7)
           "let f = proc (x y z) + (x, * (y, z)) in (f 1 2 3)"
  , testError "Too many parameters" "(proc () 1 1)"
  , testError "Too many arguments" "(proc (x y) +(x, y) 1)"
  ]

initEnv :: Environment
initEnv = initEnvironment [("i", DenoNum 1), ("v", DenoNum 5), ("x", DenoNum 10)]

testEq :: String -> ExpressedValue -> String -> Test
testEq msg expect input = TestCase $
  assertEqual msg (Right expect) evalRes
  where
    evalRes = case runParser expression "Test equal case" input of
      Right pRes  -> valueOf pRes initEnv endCont
      Left pError -> Left $ show pError

testError :: String -> String -> Test
testError msg input = TestCase $
  assertBool msg evalRes
  where
    evalRes = case runParser expression "Test equal case" input of
      Right pRes  -> case valueOf pRes initEnv endCont of
                       Left _  -> True
                       Right _ -> False
      Left _ -> False


testNoBound :: String -> String -> Test
testNoBound msg input = TestCase $
  assertEqual msg noBound True
  where
    noBound = case runParser expression "Test nobound case" input of
      Right pRes -> case valueOf pRes initEnv endCont of
                      Left s -> case stripPrefix "Not in scope: " s of
                                  Nothing -> False
                                  _       -> True
                      _ -> False
      _ -> False


