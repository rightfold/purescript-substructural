module Data.Tuple.Sub
  ( dropSnd
  , dropFst
  ) where

import Data.Function.Sub (class Drop, type (-*), drop)
import Data.Tuple (Tuple, fst, snd)
import Prelude

-- | Drop the second element of a tuple.
dropSnd :: ∀ a b. Drop b => Tuple a b -* a
dropSnd = dropSndFFI drop fst snd

-- | Drop the first element of a tuple.
dropFst :: ∀ a b. Drop a => Tuple a b -* b
dropFst = dropFstFFI drop fst snd

foreign import dropSndFFI
  :: ∀ a b
   . (b -* Unit)
  -> (∀ l r. Tuple l r -> l)
  -> (∀ l r. Tuple l r -> r)
  -> Tuple a b
  -* a

foreign import dropFstFFI
  :: ∀ a b
   . (a -* Unit)
  -> (∀ l r. Tuple l r -> l)
  -> (∀ l r. Tuple l r -> r)
  -> Tuple a b
  -* b
