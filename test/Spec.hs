import qualified ExplicitRefs.Suites as Explicit
import qualified LetLang.Suites      as Let
import qualified LetRecLang.Suites   as LetRec
import qualified NamelessIntp.Suites as Nameless
import qualified ProcLang.Suites     as Proc
import           Test.HUnit

main :: IO ()
main = do
  counts <- runTestTT $ TestList
    [ Let.tests
    , Proc.tests
    , LetRec.tests
    , Nameless.tests
    , Explicit.tests
    ]
  return ()
