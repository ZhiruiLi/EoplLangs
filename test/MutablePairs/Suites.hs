module MutablePairs.Suites where

import qualified MutablePairs.EvaluatorSuite as EvalTest
import qualified MutablePairs.ParserSuite    as ParseTest
import           Test.HUnit

tests = TestList [ EvalTest.tests, ParseTest.tests ]
