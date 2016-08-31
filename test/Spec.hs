import qualified EvaluatorSuite as EvalTest
import qualified ParserSuite    as ParseTest

import           Test.HUnit

main :: IO ()
main = do
  counts <- runTestTT $ TestList
    [ ParseTest.tests
    , EvalTest.tests
    ]
  return ()
