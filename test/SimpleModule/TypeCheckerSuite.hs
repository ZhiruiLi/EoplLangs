module SimpleModule.TypeCheckerSuite
( tests
) where

import           Data.List                (stripPrefix)
import           SimpleModule.Data
import           SimpleModule.Parser      (program)
import           SimpleModule.TypeChecker (typeOfProgram)
import           Test.HUnit
import           Text.Megaparsec          (parseErrorPretty, runParser)

tests :: Test
tests = TestList
  [ testBase
  , testOp
  , testLet
  , testProc
  , testCond
  ]

testBase :: Test
testBase = TestList
  [ testEq "Type of const value" TypeInt "3"
  , testError "Unbound var" (UnboundVar "x") "x"
  ]

testOp :: Test
testOp = TestList
  [ testEq "Type of num to num bin op" TypeInt "+(1, 2)"
  , testEq "Type of num to bool bin op" TypeBool "less?(1, 2)"
  , testEq "Type of num to bool unary op" TypeBool "zero?(5)"
  , testEq "Type of num to num unary op" TypeInt "minus(5)"
  ]

testLet :: Test
testLet = TestList
  [ testEq "Type of var" TypeInt "let x = 1 in x"
  , testEq "Type of letrec expression 1"
           TypeInt
           $ unlines
             [ "letrec int f(x: int, y: int) = 3"
             , "       bool g(x: bool) = x      in"
             , "(f 1 2)"
             ]
  , testEq "Type of letrec expression 2"
           (TypeProc [TypeInt, TypeInt] TypeInt)
           $ unlines
             [ "letrec int f(x: int, y: int) = 3"
             , "       bool g(x: bool) = x      in"
             , "f"
             ]
  ]

testProc :: Test
testProc = TestList
  [ testEq "Type of procedure with 0 argument"
           (TypeProc [] TypeInt)
           "proc () 3"
  , testEq "Type of procedure with 1 argument"
           (TypeProc [TypeInt] TypeBool)
           "proc (x: int) zero?(x)"
  , testEq "Type of procedure with more arguments"
           (TypeProc [TypeInt, TypeInt, TypeBool] TypeInt)
           "proc (x: int, y: int, z: bool) +(x, y)"
  , testEq "Type of high order procedure"
           (TypeProc [TypeProc [TypeInt] TypeInt, TypeInt] TypeInt)
           "proc (f: ((int) -> int), i: int) (f i)"
  , testError "Error for calling not procedure value"
              (CallNotProcVal TypeInt)
              "let x = 1 in (x 3)"
  , testError "Error for parameters type mismatch"
              (ParamsTypeMismatch
                [TypeBool]
                [TypeInt]
                (VarExpr "f"))
              "let f = proc (x: bool) x in (f 5)"
  , testError "Error for parameters num mismatch"
              (ParamsTypeMismatch
                [TypeInt]
                [TypeInt, TypeInt]
                (VarExpr "f"))
              "let f = proc (x: int) x in (f 5 3)"
  ]

testCond :: Test
testCond = TestList
  [ testEq "Type of condition expression"
           TypeInt
           $ unlines
             [ "cond zero?(3) ==> 4"
             , "     zero?(4) ==> 5"
             , "end"
             ]
  , testError "Eroor cause by mismatch branch types"
              (TypeMismatch TypeBool TypeInt (ConstExpr (ExprNum 3)))
              $ unlines
                [ "cond zero?(3) ==> zero?(4)"
                , "     zero?(4) ==> 3"
                , "end"
                ]
  ]

testEq :: String -> Type -> String -> Test
testEq msg expect input = TestCase $
  assertEqual msg (Right expect) tryCheck
  where
    tryCheck = case runParser program "test equal" input of
      Left err   -> Left . TypeDefaultError $ parseErrorPretty err
      Right prog -> typeOfProgram prog


testError :: String -> TypeError -> String -> Test
testError msg expect input = TestCase $
  assertEqual msg (Left expect) tryCheck
  where
    tryCheck = case runParser program "test error" input of
      Left err   -> Left . TypeDefaultError $ parseErrorPretty err
      Right prog -> typeOfProgram prog


