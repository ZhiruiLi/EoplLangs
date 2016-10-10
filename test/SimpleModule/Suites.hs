module SimpleModule.Suites where

import qualified SimpleModule.EvaluatorSuite   as EvalTest
import qualified SimpleModule.ParserSuite      as ParseTest
import qualified SimpleModule.TypeCheckerSuite as CheckerTest
import           Test.HUnit

tests = TestList [ EvalTest.tests
                 , ParseTest.tests
                 , CheckerTest.tests
                 ]
