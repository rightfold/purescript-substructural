module Test.Data.Array.Unique
  ( spec
  ) where

import Data.Array.Unique (UniqueArray, empty, fromShared, isEmpty, length, reverse, singleton, snoc, toShared)
import Data.Function.Sub (type (-*), borrow, runShared)
import Data.Tuple (Tuple)
import Data.Tuple.Sub (dropFst)
import Prelude
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: ∀ eff. Spec eff Unit
spec = describe "Data.Array.Unique" do
  it "empty" $
    runShared (empty >>> toShared) unit
    `shouldEqual` ([] :: Array Int)

  it "singleton" $
    runShared (singleton >>> toShared) 1
    `shouldEqual` [1]

  it "snoc" $
    runShared (singleton >>> borrow length >>> snoc >>> toShared) 2
    `shouldEqual` [2, 1]

  describe "isEmpty" do
    it "true" $
      let dropFst' = dropFst :: Tuple (UniqueArray Int) Boolean -* Boolean in
      runShared (empty >>> borrow isEmpty >>> dropFst') unit
      `shouldEqual` true
    it "false" $
      runShared (singleton >>> borrow isEmpty >>> dropFst) 1
      `shouldEqual` false

  it "reverse" $
    runShared (fromShared >>> reverse >>> reverse >>> reverse >>> toShared) [1, 2, 3]
    `shouldEqual` [3, 2, 1]
