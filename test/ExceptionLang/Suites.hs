module ExceptionLang.Suites where

import qualified ExceptionLang.EvaluatorSuite as EvalTest
import qualified ExceptionLang.ParserSuite    as ParseTest
import           Test.HUnit

tests = TestList [ EvalTest.tests, ParseTest.tests ]
