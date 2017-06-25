module Test.Data.Function.Sub
  ( spec
  ) where

import Data.Function.Sub (clone, fst', runShared, snd')
import Prelude
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: ∀ eff. Spec eff Unit
spec = describe "Data.Function.Sub" do
  it "fst'" $ runShared (clone >>> fst') 1 `shouldEqual` 1
  it "snd'" $ runShared (clone >>> snd') 1 `shouldEqual` 1
