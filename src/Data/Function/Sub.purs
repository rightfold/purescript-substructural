module Data.Function.Sub
  ( kind Capability
  , CLONE
  , DROP

  , class Clone, clone
  , class Drop, drop
  , unsafeClone
  , unsafeDrop

  , Sub
  , Linear
  , Affine
  , Relevant
  , type (-*)
  , type (-!)
  , type (-+)
  ) where

import Data.Tuple (Tuple(..))
import Prelude

--------------------------------------------------------------------------------

foreign import kind Capability

foreign import data CLONE :: Capability
foreign import data DROP :: Capability

--------------------------------------------------------------------------------

class Clone a where
  clone :: a -+ Tuple a a

class Drop a where
  drop :: a -! Unit

unsafeClone :: ∀ a. a -+ Tuple a a
unsafeClone = Sub \a -> Tuple a a

unsafeDrop :: ∀ a. a -! Unit
unsafeDrop = Sub \_ -> unit

instance cloneVoid :: Clone Void where clone = unsafeClone
instance dropVoid :: Drop Void where drop = unsafeDrop

instance cloneUnit :: Clone Unit where clone = unsafeClone
instance dropUnit :: Drop Unit where drop = unsafeDrop

instance cloneBoolean :: Clone Boolean where clone = unsafeClone
instance dropBoolean :: Drop Boolean where drop = unsafeDrop

instance cloneChar :: Clone Char where clone = unsafeClone
instance dropChar :: Drop Char where drop = unsafeDrop

instance cloneInt :: Clone Int where clone = unsafeClone
instance dropInt :: Drop Int where drop = unsafeDrop

instance cloneNumber :: Clone Number where clone = unsafeClone
instance dropNumber :: Drop Number where drop = unsafeDrop

instance cloneString :: Clone String where clone = unsafeClone
instance dropString :: Drop String where drop = unsafeDrop

instance cloneArray :: Clone (Array a) where clone = cloneArrayFFI Tuple
instance dropArray :: Drop (Array a) where drop = unsafeDrop

foreign import cloneArrayFFI
  :: ∀ a
   . (∀ l r. l -> r -> Tuple l r)
  -> Array a
  -+ Tuple (Array a) (Array a)

--------------------------------------------------------------------------------

newtype Sub (c :: # Capability) a b = Sub (a -> b)

derive newtype instance semigroupoidLinear :: Semigroupoid (Sub c)
derive newtype instance categoryLinear :: Category (Sub c)

type Linear = Sub ()
type Affine = Sub (drop :: DROP)
type Relevant = Sub (clone :: CLONE)

infixr 4 type Linear as -*
infixr 4 type Affine as -!
infixr 4 type Relevant as -+
