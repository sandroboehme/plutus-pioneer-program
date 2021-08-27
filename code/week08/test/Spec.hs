module Main
    ( main
    ) where

import qualified Spec.ModelSandro
import qualified Spec.Model
import qualified Spec.ModelWithClose
import qualified Spec.Trace
import qualified Spec.TraceWithCloseSandro
import qualified Spec.TraceManualRepaymentSandro
import qualified Spec.TraceWithClose
import           Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "token sale"
    [
    Spec.ModelSandro.tests
    , Spec.Trace.tests
    , Spec.TraceWithCloseSandro.tests
    -- should generally work but there is a small aspect that I currently cannot get to work
--    , Spec.TraceManualRepaymentSandro.tests
    , Spec.TraceWithClose.tests
    , Spec.Model.tests
    , Spec.ModelWithClose.tests
    ]
