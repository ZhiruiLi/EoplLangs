module ExplicitRefs.EvaluatorSuite
( tests
) where

import           Control.Monad.Trans.State.Lazy (evalStateT)
import           Data.List                      (stripPrefix)
import           ExplicitRefs.Data
import           ExplicitRefs.Evaluator
import           ExplicitRefs.Parser            (expression)
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
  , testRef
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

testRef :: Test
testRef = TestList
  [ testEq "Eval ref expressions"
           (ExprNum 18)
           $ unlines
             [ "letrec double (x) ="
             , "  if zero?(x) then 0 else + (2, (double - (x, 1)))"
             , "in let x = newref(555)"
             , "       y = newref(3)"
             , "   in begin setref(y, (double deref(y)));"
             , "            setref(x, (double deref(y)));"
             , "            + (deref(y), deref(x))"
             , "      end"
             ]
  ]

initEnv :: Environment
initEnv = initEnvironment [("i", ExprNum 1), ("v", ExprNum 5), ("x", ExprNum 10)]

testEq :: String -> ExpressedValue -> String -> Test
testEq msg expect input = TestCase $
  assertEqual msg (Right expect) evalRes
  where
    evalRes = case runParser expression "Test equal case" input of
      Right pRes  -> evalStateT (valueOf pRes initEnv) initStore
      Left pError -> Left $ show pError

testError :: String -> String -> Test
testError msg input = TestCase $
  assertBool msg evalRes
  where
    evalRes = case runParser expression "Test equal case" input of
      Right pRes  ->
        case evalStateT (valueOf pRes initEnv) initStore of
          Left _  -> True
          Right _ -> False
      Left _ -> False


testNoBound :: String -> String -> Test
testNoBound msg input = TestCase $
  assertEqual msg noBound True
  where
    noBound = case runParser expression "Test nobound case" input of
      Right pRes ->
        case evalStateT (valueOf pRes initEnv) initStore of
          Left s ->
            case stripPrefix "Not in scope: " s of
              Nothing -> False
              _       -> True
          _ -> False
      _ -> False


