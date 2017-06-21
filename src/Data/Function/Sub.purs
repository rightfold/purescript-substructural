module Data.Function.Sub
  ( class Clone, clone
  , class Drop, drop
  , unsafeClone
  , unsafeDrop
  , fst'
  , snd'

  , Sub
  , type (-*)
  ) where

import Data.Tuple (Tuple(..), fst, snd)
import Prelude

--------------------------------------------------------------------------------

-- | Values which can be cloned. Instances of `Clone` must satisfy the
-- | following laws:
-- |
-- | - Clone: `fst' <<< clone = snd' <<< clone = id`
class Clone a where
  clone :: a -* Tuple a a

-- | Values which can be dropped.
class Drop a where
  drop :: a -* Unit

-- | Unsafely clone a value.
unsafeClone :: ∀ a. a -* Tuple a a
unsafeClone = unsafeCloneFFI Tuple

-- | Unsafely drop a value.
foreign import unsafeDrop :: ∀ a. a -* Unit

-- | Drop the second element of a tuple.
fst' :: ∀ a b. Drop b => Tuple a b -* a
fst' = fst'FFI drop fst snd

-- | Drop the first element of a tuple.
snd' :: ∀ a b. Drop a => Tuple a b -* b
snd' = snd'FFI drop fst snd

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

instance cloneArray :: (Clone a) => Clone (Array a) where
  clone = cloneArrayFFI clone Tuple fst snd
instance dropArray :: (Drop a) => Drop (Array a) where
  drop = dropArrayFFI drop

foreign import unsafeCloneFFI
  :: ∀ a
   . (∀ l r. l -> r -> Tuple l r)
  -> a
  -* Tuple a a

foreign import fst'FFI
  :: ∀ a b
   . (b -* Unit)
  -> (∀ l r. Tuple l r -> l)
  -> (∀ l r. Tuple l r -> r)
  -> Tuple a b
  -* a

foreign import snd'FFI
  :: ∀ a b
   . (a -* Unit)
  -> (∀ l r. Tuple l r -> l)
  -> (∀ l r. Tuple l r -> r)
  -> Tuple a b
  -* b

foreign import cloneArrayFFI
  :: ∀ a
   . (a -* Tuple a a)
  -> (∀ l r. l -> r -> Tuple l r)
  -> (∀ l r. Tuple l r -> l)
  -> (∀ l r. Tuple l r -> r)
  -> Array a
  -* Tuple (Array a) (Array a)

foreign import dropArrayFFI :: ∀ a. (a -* Unit) -> Array a -* Unit

--------------------------------------------------------------------------------

-- | A function.
foreign import data Sub :: Type -> Type -> Type

instance semigroupoidLinear :: Semigroupoid Sub where
  compose = composeFFI

instance categoryLinear :: Category Sub where
  id = idFFI

infixr 4 type Sub as -*

foreign import composeFFI :: ∀ a b c. Sub b c -> Sub a b -> Sub a c
foreign import idFFI :: ∀ a. Sub a a
