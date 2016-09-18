module ContinuationPassing.Suites where

import qualified ContinuationPassing.EvaluatorSuite as EvalTest
import qualified ContinuationPassing.ParserSuite    as ParseTest
import           Test.HUnit

tests = TestList [ EvalTest.tests, ParseTest.tests ]
