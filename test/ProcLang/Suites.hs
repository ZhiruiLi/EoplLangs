module ProcLang.Suites where

import qualified ProcLang.EvaluatorSuite as EvalTest
import qualified ProcLang.ParserSuite    as ParseTest
import           Test.HUnit

tests = TestList [ EvalTest.tests, ParseTest.tests ]
