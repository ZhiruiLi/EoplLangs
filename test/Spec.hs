import qualified CallByName.Suites          as CallByName
import qualified CallByNeed.Suites          as CallByNeed
import qualified CallByReference.Suites     as CallByReference
import qualified ContinuationPassing.Suites as CPassing
import qualified ExceptionLang.Suites       as Exception
import qualified ExplicitRefs.Suites        as Explicit
import qualified ImplicitRefs.Suites        as Implicit
import qualified ImplicitRefsCont.Suites    as ImplicitCont
import qualified LetLang.Suites             as Let
import qualified LetRecLang.Suites          as LetRec
import qualified MutablePairs.Suites        as MutablePairs
import qualified NamelessIntp.Suites        as Nameless
import qualified ProcLang.Suites            as Proc
import           Test.HUnit

main :: IO ()
main = do
  counts <- runTestTT $ TestList
    [ Let.tests
    , Proc.tests
    , LetRec.tests
    , Nameless.tests
    , Explicit.tests
    , Implicit.tests
    , MutablePairs.tests
    , CallByReference.tests
    , CallByName.tests
    , CallByNeed.tests
    , CPassing.tests
    , Exception.tests
    , ImplicitCont.tests
    ]
  return ()
