module LetRecLang.Suites where

import qualified LetRecLang.EvaluatorSuite as EvalTest
import qualified LetRecLang.ParserSuite    as ParseTest
import           Test.HUnit

tests = TestList [ EvalTest.tests, ParseTest.tests ]
