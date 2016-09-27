module CheckedLang.Suites where

import qualified CheckedLang.EvaluatorSuite   as EvalTest
import qualified CheckedLang.ParserSuite      as ParseTest
import qualified CheckedLang.TypeCheckerSuite as CheckerTest
import           Test.HUnit

tests = TestList [ EvalTest.tests
                 , ParseTest.tests
                 , CheckerTest.tests
                 ]
