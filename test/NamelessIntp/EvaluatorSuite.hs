module NamelessIntp.EvaluatorSuite
( tests
) where

import           Data.List               (stripPrefix)
import           NamelessIntp.Data
import           NamelessIntp.Evaluator  (eval, run)
import           NamelessIntp.Parser     (expression)
import           NamelessIntp.Translator (translate)
import           Test.HUnit
import           Text.Megaparsec

tests :: Test
tests = TestList
  [ testEq "Eval const" (ExprNum 3) "3"
  , testEq "Eval bounded var" (ExprNum 5) "let v = 5 in v"
  , testNoBound "Eval no-bounded var" "let v = 5 in w"
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
           (ExprNum 2)
           $ unlines
             [ "let x = 30"
             , "in let y = -(x,2)"
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

testProc :: Test
testProc = TestList
  [ testEq "Eval proc and call expression (1 parameter)"
           (ExprNum 2)
           "(proc (x) + (x, x) 1)"
  , testEq "Eval letrec expression"
           (ExprNum 6)
           $ unlines
             [ "letrec double (x) = "
             , "  if zero?(x) then 0 else + (2, (double - (x, 1)))"
             , "in (double 3)"
             ]
  ]

testEq :: String -> ExpressedValue -> String -> Test
testEq msg expect input = TestCase $
  assertEqual msg (Right expect) (run input)

testError :: String -> String -> Test
testError msg input = TestCase $
  assertEqual msg Nothing res
  where
    res = case runParser expression "Test equal case" input of
      Left msg -> Just $ show msg
      Right expr -> case translate expr emptySEnv of
        Left msg -> Just msg
        Right nameless -> case eval nameless of
          Right rightRes -> Just $ "Evaluate result: " `mappend` show rightRes
          Left _ -> Nothing

testNoBound :: String -> String -> Test
testNoBound msg input = TestCase $
  assertEqual msg Nothing res
  where
    res = case runParser expression "Test equal case" input of
      Left msg -> Just $ show msg
      Right expr -> case translate expr emptySEnv of
        Left msg -> case stripPrefix "Not in scope: " msg of
            Nothing -> Just msg
            _       -> Nothing
        Right res -> Just $ "Translate result: " `mappend` show res


