module ThreadsLang.Suites where

import           Test.HUnit
import qualified ThreadsLang.EvaluatorSuite as EvalTest
import qualified ThreadsLang.ParserSuite    as ParseTest

tests = TestList [ EvalTest.tests, ParseTest.tests ]
