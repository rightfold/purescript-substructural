module Data.Array.Sub
  ( reverse
  ) where

import Data.Function.Sub (type (-*))

-- | Reverse an array in-place.
foreign import reverse :: ∀ a. Array a -* Array a
