module InferredLang.TypeCheckerSuite
( tests
) where

import           Data.List                (stripPrefix)
import           InferredLang.Data
import           InferredLang.Parser      (expression)
import           InferredLang.TypeChecker (typeOfExpression)
import           Test.HUnit
import           Text.Megaparsec          (parseErrorPretty, runParser)

constNum :: Integer -> Expression
constNum = ConstExpr . ExprNum

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
  , testEq "Simple type for applying letrec expression"
           TypeInt
           "letrec int f(x: int, y: int) = 3 in (f 1 2)"
  , testEq "Simple infer type for applying letrec expression"
           TypeInt
           "letrec ? f(x: ?, y: ?) = +(x, y) in (f 1 2)"
  , testEq "Polymorphic infer type for applying letrec expression"
           TypeInt
           "letrec ? f(x: ?, y: ?) = x in (f 1 2)"
  , testEq "Type of function in letrec expression"
           (TypeProc [TypeInt] TypeInt)
           "letrec int f(x: int) = 3 in f"
  , testEq "Type of polymorphic function in letrec expression"
           (TypeProc [TypeInt] TypeBool)
           "letrec ? f(x: ?) = zero?(x) in f"
  , testEq "Type of letrec expression (recursive)"
           TypeInt
           $ unlines
             [ "letrec int double(x: int)"
             , "        = if zero?(x) then 0 else -((double -(x,1)), -2)"
             , "in (double 6)"
             ]
  , testEq "Inferr type of letrec expression (recursive)"
           TypeInt
           $ unlines
             [ "letrec ? double(x: ?)"
             , "        = if zero?(x) then 0 else -((double -(x,1)), -2)"
             , "in (double 6)"
             ]
  , testEq "Inferr type of function letrec expression (recursive)"
           (TypeProc [TypeInt] TypeInt)
           $ unlines
             [ "letrec ? double(x: ?)"
             , "        = if zero?(x) then 0 else -((double -(x,1)), -2)"
             , "in double"
             ]
  , testEq "Inferr type of function letrec expression (recursive)"
           (TypeProc [TypeInt] TypeInt)
           $ unlines
             [ "letrec ? double(x: ?)"
             , "        = if zero?(x) then 0 else -((double -(x,1)), -2)"
             , "in double"
             ]
  , testEq "Type of letrec with multi parameters"
           TypeInt
           $ unlines
             [ "letrec int double(x: int, dummy: int)"
             , "        = if zero?(x) then 0 else -((double -(x,1) dummy), -2)"
             , "in (double 6 10000)"
             ]
  , testEq "Infer type of letrec with multi parameters"
           TypeInt
           $ unlines
             [ "letrec ? double(x: ?, dummy: ?)"
             , "        = if zero?(x) then 0 else -((double -(x,1) dummy), -2)"
             , "in (double 6 10000)"
             ]
  , testEq "Infer type of function with multi parameters in letrec expression"
           (TypeProc [TypeInt, TypeVar 1] TypeInt)
           $ unlines
             [ "letrec ? double(x: ?, dummy: ?)"
             , "        = if zero?(x) then 0 else -((double -(x,1) dummy), -2)"
             , "in double"
             ]
  , testEq "Type of co-recursion body in letrec expression"
           TypeInt
           $ unlines
             [ "letrec"
             , "  int even(x: int) = if zero?(x) then 1 else (odd -(x,1))"
             , "  int odd(x: int) = if zero?(x) then 0 else (even -(x,1))"
             , "in (odd 13)"
             ]
  , testEq "Infer type of co-recursion body in letrec expression"
           TypeInt
           $ unlines
             [ "letrec"
             , "  ? even(x: ?) = if zero?(x) then 1 else (odd -(x,1))"
             , "  ? odd(x: ?) = if zero?(x) then 0 else (even -(x,1))"
             , "in (odd 13)"
             ]
  , testEq "Infer type of co-recursion function in letrec expression"
           (TypeProc [TypeInt] TypeInt)
           $ unlines
             [ "letrec"
             , "  ? even(x: ?) = if zero?(x) then 1 else (odd -(x,1))"
             , "  ? odd(x: ?) = if zero?(x) then 0 else (even -(x,1))"
             , "in odd"
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
  , testEq "Infer type of procedure with 1 argument"
           (TypeProc [TypeInt] TypeBool)
           "proc (x: ?) zero?(x)"
  , testEq "Type of procedure with more arguments"
           (TypeProc [TypeInt, TypeInt, TypeBool] TypeInt)
           "proc (x: int, y: int, z: bool) +(x, y)"
  , testEq "Infer type of procedure with more arguments"
           (TypeProc [TypeInt, TypeInt, TypeVar 2] TypeInt)
           "proc (x: ?, y: ?, z: ?) +(x, y)"
  , testEq "Type of high order procedure"
           (TypeProc [TypeProc [TypeInt] TypeInt, TypeInt] TypeInt)
           "proc (f: ((int) -> int), i: int) (f i)"
  , testEq "Infer type of higher order procedure"
           (TypeProc [TypeProc [TypeInt] TypeInt, TypeInt] TypeInt)
           "proc(f: ?, x: ?) +((f x), x)"
  , testEq "Infer type when applying polymorphic procedure on concrete value"
           TypeInt
           "(proc(x: ?, y: ?) x 1 zero?(3))"
  , testError "Error for calling not procedure value"
              (TypeUnifyError TypeInt (TypeProc [TypeInt] (TypeVar 0)) (CallExpr (VarExpr "x") [constNum 3]))
              "let x = 1 in (x 3)"
  , testError "Error for parameters type mismatch"
              (TypeUnifyError TypeBool TypeInt (CallExpr (VarExpr "f") [constNum 5]))
              "let f = proc (x: bool) x in (f 5)"
  , testError "Error for parameters num mismatch"
               (TypeUnifyError (TypeProc [TypeInt] TypeInt) (TypeProc [TypeInt, TypeInt] (TypeVar 0)) (CallExpr (VarExpr "f") [constNum 5, constNum 3]))
              "let f = proc (x: int) x in (f 5 3)"
  ]

testCond :: Test
testCond = TestList
  [ testEq "Type of condition expression"
           TypeInt
           "if zero?(3) then 4 else 5"
  , testError "Eroor cause by mismatch branch types"
              (TypeUnifyError TypeBool TypeInt (IfExpr (UnaryOpExpr IsZero (constNum 3)) (UnaryOpExpr IsZero (constNum 4)) (constNum 3)))
              "if zero?(3) then zero?(4) else 3"
  ]

testEq :: String -> Type -> String -> Test
testEq msg expect input = TestCase $
  assertEqual msg (Right expect) tryCheck
  where
    tryCheck = case runParser expression "test equal" input of
      Left err   -> Left . TypeDefaultError $ parseErrorPretty err
      Right expr -> typeOfExpression expr

testError :: String -> TypeError -> String -> Test
testError msg expect input = TestCase $
  assertEqual msg (Left expect) tryCheck
  where
    tryCheck = case runParser expression "test error" input of
      Left err   -> Left . TypeDefaultError $ parseErrorPretty err
      Right expr -> typeOfExpression expr
