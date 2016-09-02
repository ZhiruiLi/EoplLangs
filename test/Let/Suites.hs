module Let.Suites where

import qualified Let.EvaluatorSuite as EvalTest
import qualified Let.ParserSuite    as ParseTest
import           Test.HUnit

tests = TestList [ EvalTest.tests, ParseTest.tests ]
