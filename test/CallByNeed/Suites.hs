module CallByNeed.Suites where

import qualified CallByNeed.EvaluatorSuite as EvalTest
import qualified CallByNeed.ParserSuite    as ParseTest
import           Test.HUnit

tests = TestList [ EvalTest.tests, ParseTest.tests ]
