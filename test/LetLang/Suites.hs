module LetLang.Suites where

import qualified LetLang.EvaluatorSuite as EvalTest
import qualified LetLang.ParserSuite    as ParseTest
import           Test.HUnit

tests = TestList [ EvalTest.tests, ParseTest.tests ]
