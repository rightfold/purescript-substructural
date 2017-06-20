module Data.Array.Sub
  ( runSubArray
  , borrowArray
  , reverse
  ) where

import Control.Monad.Eff (Eff)
import Control.Monad.ST (ST)
import Data.Array.ST (STArray)
import Data.Function.Sub (Sub)
import Prelude

-- | Apply a function to the copy of an array.
foreign import runSubArray :: ∀ a b c. Sub c (Array a) b -> Array a -> b

-- | Apply a mutating operation, without copying the array.
foreign import borrowArray
  :: ∀ a c
   . (∀ s. STArray s a -> Eff (st :: ST s) Unit)
  -> Sub c (Array a) (Array a)

-- | Reverse an array in-place.
foreign import reverse :: ∀ c a. Sub c (Array a) (Array a)
