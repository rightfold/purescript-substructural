module Data.Array.Sub
  ( runSubArray
  , reverse
  ) where

import Data.Function.Sub (Sub)

-- | Apply a function to the copy of an array.
foreign import runSubArray :: ∀ a b c. Sub c (Array a) b -> Array a -> b

-- | Reverse an array in-place.
foreign import reverse :: ∀ c a. Sub c (Array a) (Array a)
