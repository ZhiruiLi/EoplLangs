import qualified Let.Suites as Let
import           Test.HUnit

main :: IO ()
main = do
  counts <- runTestTT $ TestList
    [ Let.tests
    ]
  return ()
