module ExplicitRefs.Suites where

import qualified ExplicitRefs.EvaluatorSuite as EvalTest
import qualified ExplicitRefs.ParserSuite    as ParseTest
import           Test.HUnit

tests = TestList [ EvalTest.tests, ParseTest.tests ]
