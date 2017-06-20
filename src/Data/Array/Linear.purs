module Data.Array.Linear
  ( borrowArray
  ) where

import Control.Monad.Eff (Eff)
import Control.Monad.ST (ST)
import Data.Array.ST (STArray)
import Data.Function.Linear (type (-*))

foreign import borrowArray
  :: ∀ a
   . (∀ s. STArray s a -> Eff (st :: ST s) (STArray s a))
  -> Array a
  -* Array a
