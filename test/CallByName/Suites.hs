module CallByName.Suites where

import qualified CallByName.EvaluatorSuite as EvalTest
import qualified CallByName.ParserSuite    as ParseTest
import           Test.HUnit

tests = TestList [ EvalTest.tests, ParseTest.tests ]
