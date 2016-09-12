module ImplicitRefs.Suites where

import qualified ImplicitRefs.EvaluatorSuite as EvalTest
import qualified ImplicitRefs.ParserSuite    as ParseTest
import           Test.HUnit

tests = TestList [ EvalTest.tests, ParseTest.tests ]
