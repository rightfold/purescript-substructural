module Data.Function.Linear
  ( Linear
  , type (⊸)
  ) where

import Prelude

newtype Linear a b = Linear (a -> b)

infixr 4 type Linear as ⊸

derive newtype instance semigroupoidLinear :: Semigroupoid Linear
derive newtype instance categoryLinear :: Category Linear
