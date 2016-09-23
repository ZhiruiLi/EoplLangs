module ThreadsLang.EvaluatorSuite
( tests
) where

import           Control.Monad.Except
import           Control.Monad.Trans.State.Lazy (evalStateT)
import           Data.List                      (stripPrefix)
import           Test.HUnit
import           Text.Megaparsec
import           ThreadsLang.Data
import           ThreadsLang.Evaluator
import           ThreadsLang.Parser             (expression)

tests :: Test
tests = TestList
  [ testBasic
  , testOps
  , testLet
  , testCond
  , testProc
  , testRef
  , testSpawn
  ]

testBasic :: Test
testBasic = TestList
  [ testEq "Eval const" (ExprNum 3) "3"
  , testEq "Eval bounded var" (ExprNum 5) "let v = 5 in v"
  , testErr "Eval no-bounded var" (UnboundVar "y") "let x = 5 in y"
  ]

testOps :: Test
testOps = TestList
  [ testEq "Eval binary num-to-num operator 1" (ExprNum 5) "-(10, 5)"
  , testEq "Eval binary num-to-num operator 2" (ExprNum 15) "+(10, 5)"
  , testEq "Eval binary num-to-bool operator (true case)"
           (ExprBool True) "greater?(4, 3)"
  , testEq "Eval binary num-to-bool operator (false case)"
           (ExprBool False) "less?(5,2)"
  , testEq "Eval unary operator" (ExprNum (-1)) "minus(1)"
  ]

testLet :: Test
testLet = TestList
  [ testEq "Eval let expression"
           (ExprNum 2)
           $ unlines
             [ "let x = 30"
             , "in let y = -(x,2)"
             , "   in -(x,y)"
             ]
  , testEq "Eval let with multi bindings 1"
           (ExprNum 1)
           $ unlines
             [ "let x = 1"
             , "    y = 2"
             , "    z = 3"
             , "in x"
             ]
  , testEq "Eval let with multi bindings 2"
           (ExprNum 3)
           $ unlines
             [ "let x = 1"
             , "    y = 2"
             , "    z = 3"
             , "in z"
             ]
  , testEq "Eval let expression with variable shadowing"
           (ExprNum 2)
           $ unlines
             [ "let x = 1"
             , "in let x = 2"
             , "   in x"
             ]
  , testEq "Eval letrec with recursion"
           (ExprNum 12)
           $ unlines
             [ "letrec double(x)"
             , "        = if zero?(x) then 0 else -((double -(x,1)), -2)"
             , "in (double 6)"
             ]
  , testEq "Eval letrec with co-recursion"
           (ExprNum 1)
           $ unlines
             [ "letrec"
             , "  even(x) = if zero?(x) then 1 else (odd -(x,1))"
             , "  odd(x) = if zero?(x) then 0 else (even -(x,1))"
             , "in (odd 13)"
             ]
  , testEq "Eval let and begin expression"
           (ExprNum 12)
           $ unlines
             [ "let times4 = 0"
             , "in begin"
             , "     set times4 = proc (x)"
             , "                    if zero?(x)"
             , "                    then 0"
             , "                    else -((times4 -(x,1)), -4);"
             , "     (times4 3)"
             , "   end"
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
  , testErr "Too many parameters"
            (ArgNumMismatch 0 [ExprNum 1])
            "(proc () 1 1)"
  , testErr "Too many arguments"
            (ArgNumMismatch 2 [ExprNum 1])
            "(proc (x y) +(x, y) 1)"
  ]

testRef :: Test
testRef = TestList
  [ testEq "Set value of a parameter should not affect the original variable."
           (ExprNum 3)
           $ unlines
             [ "let p = proc (x) set x = 4"
             , "in let a = 3"
             , "   in begin (p a); a end"
             ]
  , testEq ("Parameters of nested function call should " `mappend`
            "not refer to the same variable.")
           (ExprNum 55)
           $ unlines
             [ "let f = proc (x) set x = 44"
             , "in let g = proc (y) (f y)"
             , "   in let z = 55"
             , "      in begin (g z); z end"
             ]
  , testEq "Test implicit ref"
           (ExprNum 3)
           $ unlines
             [ "let x = 4"
             , "in begin set x = 3;"
             , "         x"
             , "   end"
             ]
  ]

testSpawn :: Test
testSpawn = TestList
  [ testEq "Test spawn 1"
           (ExprNum 1)
           $ unlines
             [ "let x = 0 in"
             , "let incX = proc() set x = + (x, 1) in"
             , "letrec buzyWait(n) = "
             , "         proc() "
             , "           if zero?(n) then 1 else (buzyWait -(n, 1)) "
             , "in begin spawn(incX);"
             , "      (buzyWait 100);"
             , "      x"
             , "   end"
             ]
  , testEq "Test spawn 2"
           (ExprNum 3)
           $ unlines
             [ "let x = 0 in"
             , "let incX = proc() set x = + (x, 1) in"
             , "letrec buzyWait(n) = "
             , "         proc() "
             , "           if zero?(n) then 1 else (buzyWait -(n, 1)) "
             , "in begin spawn(incX);"
             , "      spawn(incX);"
             , "      spawn(incX);"
             , "      (buzyWait 100);"
             , "      x"
             , "   end"
             ]
  ]


testEq :: String -> ExpressedValue -> String -> Test
testEq msg expect input = TestCase $ do
  evalRes <- run input
  assertEqual msg (Right expect) evalRes

testErr :: String -> LangError -> String -> Test
testErr msg expect input = TestCase $ do
  evalRes <- run input
  assertEqual msg (Left expect) evalRes

