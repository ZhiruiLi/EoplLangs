module InferredLang.Suites where

import qualified InferredLang.EvaluatorSuite   as EvalTest
import qualified InferredLang.ParserSuite      as ParseTest
import qualified InferredLang.TypeCheckerSuite as CheckerTest
import           Test.HUnit

tests = TestList [ EvalTest.tests
                 , ParseTest.tests
                 , CheckerTest.tests
                 ]
