module LetLang.ParserSuite
( tests
) where

import           LetLang.Data.Expression
import           LetLang.Data.Program
import           LetLang.Parser
import           Test.HUnit.Base
import           Text.Megaparsec
import           Text.Megaparsec.String

tests :: Test
tests = TestList
  [ TestLabel "Test const expression" testConstExpr
  , TestLabel "Test binary operate expression" testBinOpExpr
  , TestLabel "Test isZero expression" testIsZeroExpr
  , TestLabel "Test if expression" testIfExpr
  , TestLabel "Test var expression" testVarExpr
  , TestLabel "Test let expression" testLetExpr
  , TestLabel "Test minus expression" testMinusExpr
  , TestLabel "Test expression" testExpression
  , TestLabel "Test parse program" testParseProgram
  ]

parserEqCase :: (Eq a, Show a) => Parser a -> String -> a -> String -> Test
parserEqCase parser msg expect input =
  TestCase $ assertEqual msg (Right expect) runP
  where runP = runParser parser "Test equal case" input

parserFailCase :: (Eq a, Show a) => Parser a -> String -> String -> Test
parserFailCase parser msg input =
  TestCase $ assertBool msg (isLeft runP)
  where
    isLeft (Left _)  = True
    isLeft (Right _) = False
    runP = runParser parser "Test fail case" input

testConstExpr :: Test
testConstExpr = TestList
  [ testEq "Parse single number" (ConstExpr 5) "5"
  , testEq "Parse multi-numbers" (ConstExpr 123) "123"
  , testFail "Parse negative numbers should fail" "-3"
  ]
  where
    testEq = parserEqCase constExpr
    testFail = parserFailCase constExpr

testBinOpExpr :: Test
testBinOpExpr = TestList
  [ testEq "Parse '-' expression (no space)"
           (BinOpExpr Sub (ConstExpr 3) (ConstExpr 4))
           "-(3,4)"
  , testEq "Parse '*' expression (with spaces)"
           (BinOpExpr Mul (ConstExpr 10) (ConstExpr 24))
           "* (  10  ,    24 )"
  , testEq "Parse binary num-to-bool expression"
           (BinOpExpr Gt (ConstExpr 1) (ConstExpr 2))
           "greater?(1, 2)"
  ]
  where testEq = parserEqCase binOpExpr

testIsZeroExpr :: Test
testIsZeroExpr = TestList
  [ testEq "Parse isZero expression (no space)" (IsZeroExpr (ConstExpr 1)) "zero?(1)"
  , testEq "Parse isZero expression (with space)" (IsZeroExpr (ConstExpr 3)) "zero? ( 3  )"
  ]
  where testEq = parserEqCase isZeroExpr

testIfExpr :: Test
testIfExpr = TestList
  [ testEq "Parse if expression"
           (IfExpr (IsZeroExpr (ConstExpr 3)) (ConstExpr 4) (ConstExpr 5))
           "if zero?(3) then 4 else 5"
  ]
  where testEq = parserEqCase ifExpr

testVarExpr :: Test
testVarExpr = TestList
  [ testEq "Parse var expression" (VarExpr "foo") "foo"
  , testFail "Parse reserved word should fail" "then"
  ]
  where
    testEq = parserEqCase varExpr
    testFail = parserFailCase varExpr

testLetExpr :: Test
testLetExpr = TestList
  [ testEq "Parse let expression"
           (LetExpr "bar" (ConstExpr 1) (VarExpr "bar"))
           "let bar = 1 in bar"
  ]
  where testEq = parserEqCase letExpr

testMinusExpr :: Test
testMinusExpr = TestList
  [ testEq "Parse minus expression" (MinusExpr (ConstExpr 1)) "minus(1)" ]
  where testEq = parserEqCase minusExpr

testExpression :: Test
testExpression = TestList
  [ testFail "Parse negative numbers should fail" "-3"
  , testFail "Parse reserved word should fail" "then"
  , testEq "Parse complex expression"
           (LetExpr "bar"
                    (ConstExpr 1)
                    (IfExpr (IsZeroExpr (VarExpr "bar"))
                            (ConstExpr 3)
                            (VarExpr "zero")))
           "let bar = 1 in if zero? (bar) then 3 else zero"
  ]
  where
    testEq = parserEqCase expression
    testFail = parserFailCase expression

testParseProgram :: Test
testParseProgram = TestList
  [ testEq "Parse program (with spaces)"
            (Program (LetExpr "x"
                              (ConstExpr 3)
                              (VarExpr "x")))
            "let x = 3 in x"
  ]
  where
    testEq msg expect prog = TestCase $
      assertEqual msg (Right expect) (parseProgram prog)
