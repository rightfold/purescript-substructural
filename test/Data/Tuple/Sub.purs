module Test.Data.Tuple.Sub
  ( spec
  ) where

import Data.Function.Sub (clone, runShared)
import Data.Tuple.Sub (dropFst, dropSnd)
import Prelude
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: ∀ eff. Spec eff Unit
spec = describe "Data.Tuple.Sub" do
  it "dropSnd" $ runShared (clone >>> dropSnd) 1 `shouldEqual` 1
  it "dropFst" $ runShared (clone >>> dropFst) 1 `shouldEqual` 1
