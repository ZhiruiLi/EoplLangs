module NamelessIntp.Suites where

import qualified NamelessIntp.EvaluatorSuite as EvalTest
import qualified NamelessIntp.ParserSuite    as ParseTest
import           Test.HUnit

tests = TestList [ EvalTest.tests, ParseTest.tests ]
