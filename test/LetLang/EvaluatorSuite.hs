module LetLang.EvaluatorSuite
( tests
) where

import           Data.List         (stripPrefix)
import           LetLang.Data
import           LetLang.Evaluator
import           LetLang.Parser    (expression)
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

  , testLet
  , testCond
  , testEq "Eval minus" (ExprNum (-1)) "minus(1)"
  , testList
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
  , testEq "Eval let*"
           (ExprNum 2)
           $ unlines
             [ "let x = 30"
             , "in let* x = -(x,1) y = -(x,2)"
             , "   in -(x,y)"
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

testList :: Test
testList = TestList
  [ testEq "Eval empty list" (ExprList []) "emptyList"
  , testError "'car' should fail on empty list" "car(emptyList)"
  , testError "'cdr' should fail on empty list" "cdr(emptyList)"
  , testEq "'car' should get the head value of list"
           (ExprNum 1) "car(cons(1, emptyList))"
  , testEq "'cdr' should get the tail list of list"
           (ExprList []) "cdr(cons(1, emptyList))"
  , testEq "'list(...)' should construct a list (include nested)"
           (ExprList [ ExprNum 1
                    , ExprList [ ExprNum 2, ExprNum 3 ]
                    , ExprNum 4
                    ])
           "list (1, list(2, 3), 4)"
  , testEq "'list()' should construct a empty list"
           (ExprList []) "list ()"
  ]

initEnv :: Environment
initEnv = initEnvironment [("i", ExprNum 1), ("v", ExprNum 5), ("x", ExprNum 10)]

testEq :: String -> ExpressedValue -> String -> Test
testEq msg expect input = TestCase $
  assertEqual msg (Right expect) evalRes
  where
    evalRes = case runParser expression "Test equal case" input of
      Right pRes  -> valueOf pRes initEnv
      Left pError -> Left $ show pError

testError :: String -> String -> Test
testError msg input = TestCase $
  assertBool msg evalRes
  where
    evalRes = case runParser expression "Test equal case" input of
      Right pRes  -> case valueOf pRes initEnv of
                       Left _  -> True
                       Right _ -> False
      Left _ -> False


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
