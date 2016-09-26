module LetRecLang.ParserSuite
( tests
) where

import           LetRecLang.Data
import           LetRecLang.Parser
import           Test.HUnit.Base
import           Text.Megaparsec
import           Text.Megaparsec.String

tests :: Test
tests = TestList
  [ TestLabel "Test const expression" testConstExpr
  , TestLabel "Test binary-operator expression" testBinOpExpr
  , TestLabel "Test unary-operator expression" testUnaryOpExpr
  , TestLabel "Test condition expression" testCondExpr
  , TestLabel "Test var expression" testVarExpr
  , TestLabel "Test let expression" testLetExpr
  , TestLabel "Test expression" testExpression
  , TestLabel "Test parse program" testParseProgram
  , TestLabel "Test parse proc expression" testParseProc
  ]

constNum = ConstExpr . ExprNum
constBool = ConstExpr . ExprBool

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

testEq :: String -> Expression -> String -> Test
testEq = parserEqCase expression

testFail :: String -> String -> Test
testFail = parserFailCase expression

testConstExpr :: Test
testConstExpr = TestList
  [ testEq "Parse single number" (constNum 5) "5"
  , testEq "Parse multi-numbers" (constNum 123) "123"
  , testEq "Parse negative number" (constNum (-233)) "-233"
  ]

testBinOpExpr :: Test
testBinOpExpr = TestList
  [ testEq "Parse '-' expression (no space)"
           (BinOpExpr Sub (constNum 3) (constNum 4))
           "-(3,4)"
  , testEq "Parse '*' expression (with spaces)"
           (BinOpExpr Mul (constNum 10) (constNum 24))
           "* (  10  ,    24 )"
  , testEq "Parse binary num-to-bool expression"
           (BinOpExpr Gt (constNum 1) (constNum 2))
           "greater?(1, 2)"
  ]

testUnaryOpExpr :: Test
testUnaryOpExpr = TestList
  [ testEq "Parse isZero expression (no space)"
           (UnaryOpExpr IsZero (constNum 1)) "zero?(1)"
  , testEq "Parse isZero expression (with space)"
           (UnaryOpExpr IsZero (constNum 3)) "zero? ( 3  )"
  , testEq "Parse minus expression"
           (UnaryOpExpr Minus (constNum 1)) "minus(1)"
  ]

testCondExpr :: Test
testCondExpr = TestList
  [ testEq "Parse if expression"
           (CondExpr
             [ (UnaryOpExpr IsZero (constNum 3), constNum 4)
             , (constBool True, constNum 5) ])
           "if zero?(3) then 4 else 5"
  , testEq "Parse cond expression"
           (CondExpr
             [ (UnaryOpExpr IsZero (constNum 3), constNum 4)
             , (BinOpExpr Gt (constNum 3) (constNum 5), constNum 4)
             , (UnaryOpExpr IsZero (constNum 0), constNum 4)
             ])
           $ unlines
             [ "cond zero?(3) ==> 4\n"
             , "     greater?(3, 5) ==> 4\n"
             , "     zero?(0) ==> 4\n"
             , "end"
             ]
  ]

testVarExpr :: Test
testVarExpr = TestList
  [ testEq "Parse var expression" (VarExpr "foo") "foo"
  , testFail "Parse reserved word should fail" "then"
  ]

testLetExpr :: Test
testLetExpr = TestList
  [ testEq "Parse let expression with 0 binding"
           (LetExpr [] (VarExpr "bar"))
           "let in bar"
  , testEq "Parse let expression with 1 binding"
           (LetExpr [("bar", constNum 1)] (VarExpr "bar"))
           "let bar = 1 in bar"
  , testEq "Parse let expression with multi bindings"
           (LetExpr [("x", constNum 1), ("y", constNum 2), ("z", constNum 3)]
                    (VarExpr "bar"))
           "let x = 1 y = 2 z = 3 in bar"
  , testEq "Parse let* expression with 1 binding"
           (LetStarExpr [("x", constNum 1)]
                        (VarExpr "bar"))
           "let* x = 1 in bar"
  , testEq "Parse nested let and let*"
           (LetExpr
             [("x", constNum 30)]
             (LetStarExpr
               [ ("x", BinOpExpr Sub (VarExpr "x") (constNum 1))
               , ("y", BinOpExpr Sub (VarExpr "x") (constNum 2))
               ]
               (BinOpExpr Sub (VarExpr "x") (VarExpr "y"))))
          "let x = 30 in let* x = -(x,1) y = -(x,2) in -(x,y)"
  , testEq "Parse let recursive expression"
         (LetRecExpr
           [("double", ["xxx"], BinOpExpr Mul (constNum 2) (VarExpr "xxx"))]
           (CallExpr (VarExpr "double") [constNum 5]))
         "letrec double(xxx) = *(2, xxx) in (double 5)"
  ]

testExpression :: Test
testExpression = TestList
  [ testEq "Parse complex expression"
           (LetExpr [("bar", constNum 1)]
                    (CondExpr
                      [ (UnaryOpExpr IsZero (VarExpr "bar"), constNum 3)
                      , (constBool True, VarExpr "zero") ]))
           "let bar = 1 in if zero? (bar) then 3 else zero"
  ]

testParseProgram :: Test
testParseProgram = TestList
  [ testEq "Parse program (with spaces)"
            (Prog (LetExpr [("x", constNum 3)]
                              (VarExpr "x")))
            "let x = 3 in x"
  ]
  where
    testEq msg expect prog = TestCase $
      assertEqual msg (Right expect) (parseProgram prog)

testParseProc :: Test
testParseProc = TestList
  [ testEq "Parse proc expression"
           (ProcExpr ["x"] (VarExpr "x"))
           "proc (x) x"
  , testEq "Parse proc expression with multi parameters"
           (ProcExpr ["x", "y", "z"] (VarExpr "x"))
           "proc (x, y, z) x"
  , testEq "Parse call expression"
           (CallExpr
             (ProcExpr
               ["f"]
               (CallExpr (VarExpr "f") [CallExpr (VarExpr "f") [constNum 77]]))
             [ProcExpr ["x"] (BinOpExpr Sub (VarExpr "x") (constNum 11))])
           "(proc (f) (f (f 77)) proc (x) -(x,11))"
  ]
