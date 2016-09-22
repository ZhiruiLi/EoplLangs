module ImplicitRefsCont.Suites where

import qualified ImplicitRefsCont.EvaluatorSuite as EvalTest
import qualified ImplicitRefsCont.ParserSuite    as ParseTest
import           Test.HUnit

tests = TestList [ EvalTest.tests, ParseTest.tests ]
