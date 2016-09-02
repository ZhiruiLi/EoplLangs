module Let.EvaluatorSuite
( tests
) where

import           Data.List                   (stripPrefix)
import           Let.Data.Environment
import           Let.Data.ExpressedValue
import           Let.Evaluator
import           Let.Parser              (expression)
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
           (NumVal 1)
           $ unlines
              [ "let x = 30"
              , "in let x = -(x,1)"
              , "       y = -(x,2)"
              , "   in -(x,y)"
              ]
  , testCond
  , testEq "Eval minus" (NumVal (-1)) "minus(1)"
  , testList
  ]

testCond :: Test
testCond = TestList
  [ testEq "Eval true branch of if expression"
           (NumVal 3)
           "if zero? (0) then 3 else 4"
  , testEq "Eval false branch of if expression"
           (NumVal 4)
           "if zero? (5) then 3 else 4"
  , testError "Empty condition expression should fail" "cond end"
  , testError "A condition expression with no true predicate should fail"
              "cond zero?(5) ==> 3 greater?(5, 10) ==> 4 end"
  , testEq "Match first condition"
           (NumVal 1)
           "cond zero?(0) ==> 1 zero?(0) ==> 2 zero?(0) ==> 3 end"
  , testEq "Match third condition"
           (NumVal 3)
           "cond zero?(1) ==> 1 zero?(2) ==> 2 zero?(0) ==> 3 end"
  ]

testList :: Test
testList = TestList
  [ testEq "Eval empty list" (ListVal []) "emptyList"
  , testError "'car' should fail on empty list" "car(emptyList)"
  , testError "'cdr' should fail on empty list" "cdr(emptyList)"
  , testEq "'car' should get the head value of list"
           (NumVal 1) "car(cons(1, emptyList))"
  , testEq "'cdr' should get the tail list of list"
           (ListVal []) "cdr(cons(1, emptyList))"
  , testEq "'list(...)' should construct a list (include nested)"
           (ListVal [ NumVal 1
                    , ListVal [ NumVal 2, NumVal 3 ]
                    , NumVal 4
                    ])
           "list (1, list(2, 3), 4)"
  , testEq "'list()' should construct a empty list"
           (ListVal []) "list ()"
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
