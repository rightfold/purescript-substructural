module Data.Array.Sub
  ( borrowArray
  ) where

import Control.Monad.Eff (Eff)
import Control.Monad.ST (ST)
import Data.Array.ST (STArray)
import Data.Function.Sub (Sub)

foreign import borrowArray
  :: ∀ a c
   . (∀ s. STArray s a -> Eff (st :: ST s) (STArray s a))
  -> Sub c (Array a) (Array a)
