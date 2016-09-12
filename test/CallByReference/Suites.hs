module CallByReference.Suites where

import qualified CallByReference.EvaluatorSuite as EvalTest
import qualified CallByReference.ParserSuite    as ParseTest
import           Test.HUnit

tests = TestList [ EvalTest.tests, ParseTest.tests ]
