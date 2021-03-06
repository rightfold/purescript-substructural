module Data.Array.Unique
  ( UniqueArray

  , empty
  , singleton
  , fromShared
  , toShared

  , snoc

  , isEmpty
  , length

  , reverse
  ) where

import Data.Function.Sub (class Clone, class Drop, class Shared, type (-*), Borrow, clone, drop, liftShared)
import Data.Tuple (Tuple(..), fst, snd)
import Prelude

--------------------------------------------------------------------------------

-- | An array that has at most one live reference to it.
foreign import data UniqueArray :: Type -> Type

instance cloneUniqueArray :: (Clone a) => Clone (UniqueArray a) where
  clone = cloneUniqueArrayFFI clone Tuple fst snd

instance dropUniqueArray :: (Drop a) => Drop (UniqueArray a) where
  drop = dropUniqueArrayFFI drop

foreign import cloneUniqueArrayFFI
  :: ∀ a
   . (a -* Tuple a a)
  -> (∀ l r. l -> r -> Tuple l r)
  -> (∀ l r. Tuple l r -> l)
  -> (∀ l r. Tuple l r -> r)
  -> UniqueArray a
  -* Tuple (UniqueArray a) (UniqueArray a)

foreign import dropUniqueArrayFFI
  :: ∀ a
   . (a -* Unit)
  -> UniqueArray a
  -* Unit

--------------------------------------------------------------------------------

-- | O(1) memory. O(1) time. The empty array.
foreign import empty :: ∀ a. Unit -* UniqueArray a

-- | O(1) memory, O(1) time. A singleton array.
foreign import singleton :: ∀ a. a -* UniqueArray a

-- | O(n) memory, O(n) time. Create a unique array from a shared array.
fromShared :: ∀ a. Shared a => Array a -* UniqueArray a
fromShared = fromSharedFFI

-- | O(1) memory, O(1) time. Create a shared array from a unique array.
toShared :: ∀ a. Shared a => UniqueArray a -* Array a
toShared = toSharedFFI

foreign import fromSharedFFI :: ∀ a. Array a -* UniqueArray a
foreign import toSharedFFI :: ∀ a. UniqueArray a -* Array a

--------------------------------------------------------------------------------

-- | O(1) memory, O(1) time. Append an element to an array.
snoc :: ∀ a. Tuple (UniqueArray a) a -* UniqueArray a
snoc = snocFFI fst snd

foreign import snocFFI
  :: ∀ a
   . (∀ l r. Tuple l r -> l)
  -> (∀ l r. Tuple l r -> r)
  -> Tuple (UniqueArray a) a
  -* UniqueArray a

--------------------------------------------------------------------------------

-- | O(1) memory, O(1) time. Return whether the array is empty.
isEmpty :: ∀ a. Borrow (UniqueArray a) -* Boolean
isEmpty = liftShared (eq 0) <<< length

-- | O(1) memory, O(1) time. Return the length of an array.
foreign import length :: ∀ a. Borrow (UniqueArray a) -* Int

--------------------------------------------------------------------------------

-- | O(1) memory, O(n) time. Reverse an array.
foreign import reverse :: ∀ a. UniqueArray a -* UniqueArray a
