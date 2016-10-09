module SimpleModule.EvaluatorSuite
( tests
) where

import           Data.List              (stripPrefix)
import           SimpleModule.Data
import           SimpleModule.Evaluator
import           SimpleModule.Parser    (expression)
import           Test.HUnit

tests :: Test
tests = TestList
  [ testEq "Eval const" (ExprNum 3) "3"
  , testEq "Eval bounded var" (ExprNum 5) "let v = 5 in v"
  , testEq "Eval binary num-to-num operator" (ExprNum 5) "-(10, 5)"
  , testEq "Eval binary num-to-bool operator (true case)"
           (ExprBool True) "greater?(4, 3)"
  , testEq "Eval binary num-to-bool operator (false case)"
           (ExprBool False) "less?(5,2)"
  , testEq "Eval minus" (ExprNum (-1)) "minus(1)"
  , testLet
  , testCond
  , testProc
  , testModule
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
  , testEq "Eval letrec"
           (ExprNum 12)
           $ unlines
             [ "letrec int double(x: int)"
             , "        = if zero?(x) then 0 else -((double -(x,1)), -2)"
             , "in (double 6)"
             ]
  , testEq "Eval letrec with multi parameters"
           (ExprNum 12)
           $ unlines
             [ "letrec int double(x: int, dummy: int)"
             , "        = if zero?(x) then 0 else -((double -(x,1) dummy), -2)"
             , "in (double 6 10000)"
             ]
  , testEq "Eval co-recursion"
           (ExprNum 1)
           $ unlines
             [ "letrec"
             , "  int even(x: int) = if zero?(x) then 1 else (odd -(x,1))"
             , "  int odd(x: int) = if zero?(x) then 0 else (even -(x,1))"
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
  , testError "Empty condition expression should fail" "cond end"
  , testError "A condition expression with no true predicate should fail"
              "cond zero?(5) ==> 3 greater?(5, 10) ==> 4 end"
  , testEq "Match first condition"
           (ExprNum 1)
           "cond zero?(0) ==> 1 zero?(0) ==> 2 zero?(0) ==> 3 end"
  , testEq "Match third condition"
           (ExprNum 3)
           "cond zero?(1) ==> 1 zero?(2) ==> 2 zero?(0) ==> 3 end"
  ]

testProc :: Test
testProc = TestList
  [ testEq "Eval proc and call expression (no parameter)"
           (ExprNum 1)
           "(proc () 1)"
  , testEq "Eval proc and call expression (1 parameter)"
           (ExprNum 2)
           "(proc (x: int) + (x, x) 1)"
  , testEq "Eval proc and call expression (many parameters)"
           (ExprNum 7)
           "(proc (x: int, y: int, z: int) + (x, * (y, z)) 1 2 3)"
  , testEq "Eval named proc"
           (ExprNum 7)
           "let f = proc (x: int, y: int, z: int) + (x, * (y, z)) in (f 1 2 3)"
  , testError "Too many parameters" "(proc () 1 1)"
  , testError "Too many arguments" "(proc (x: int, y: int) +(x, y) 1)"
  ]

testModule :: Test
testModule = TestList
  [ testEq "Test program with modules"
           (ExprNum 44)
           $ unlines
             [ "module m1"
             , "interface"
             , "  [a : int"
             , "   b : int"
             , "   c : int]"
             , "body"
             , "  [a = 33"
             , "   b = 44"
             , "   c = 55]"
             , "module m2"
             , "interface"
             , "  [a : int"
             , "   b : int]"
             , "body"
             , "  [a = 66"
             , "   b = 77]"
             , "let z = 11"
             , "in -(z, -(from m1 take a, from m2 take a))"
             ]
  ]

testEq :: String -> ExpressedValue -> String -> Test
testEq msg expect input = TestCase $
  assertEqual msg (Right expect) (run input)

testError :: String -> String -> Test
testError msg input = TestCase $
  assertBool msg evalRes
  where
    evalRes = case run input of
      Left (ParseError _) -> False
      Right _             -> False
      _                   -> True
