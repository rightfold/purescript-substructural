module Data.Function.Sub
  ( Sub
  , type (-*)
  , liftShared

  , class Clone, clone
  , class Drop, drop
  , class Shared
  , unsafeClone
  , unsafeDrop
  , fst'
  , snd'

  , Borrow
  , borrow
  ) where

import Data.Tuple (Tuple(..), fst, snd)
import Prelude

--------------------------------------------------------------------------------

-- | A function.
foreign import data Sub :: Type -> Type -> Type

instance semigroupoidLinear :: Semigroupoid Sub where
  compose = composeFFI

instance categoryLinear :: Category Sub where
  id = idFFI

infixr 4 type Sub as -*

-- Lift a function over shared values into a `Sub`.
liftShared :: ∀ a b. Shared a => Shared b => (a -> b) -> a -* b
liftShared = liftSharedFFI

foreign import composeFFI :: ∀ a b c. Sub b c -> Sub a b -> Sub a c
foreign import idFFI :: ∀ a. Sub a a
foreign import liftSharedFFI :: ∀ a b. (a -> b) -> a -* b

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

-- | Values for which multiple live references can exist. `Clone` and `Drop`
-- | instances may be no-ops.
class (Clone a, Drop a) <= Shared a

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

--------------------------------------------------------------------------------

instance cloneVoid :: Clone Void where clone = unsafeClone
instance dropVoid :: Drop Void where drop = unsafeDrop
instance sharedVoid :: Shared Void

instance cloneUnit :: Clone Unit where clone = unsafeClone
instance dropUnit :: Drop Unit where drop = unsafeDrop
instance sharedUnit :: Shared Unit

instance cloneBoolean :: Clone Boolean where clone = unsafeClone
instance dropBoolean :: Drop Boolean where drop = unsafeDrop
instance sharedBoolean :: Shared Boolean

instance cloneChar :: Clone Char where clone = unsafeClone
instance dropChar :: Drop Char where drop = unsafeDrop
instance sharedChar :: Shared Char

instance cloneInt :: Clone Int where clone = unsafeClone
instance dropInt :: Drop Int where drop = unsafeDrop
instance sharedInt :: Shared Int

instance cloneNumber :: Clone Number where clone = unsafeClone
instance dropNumber :: Drop Number where drop = unsafeDrop
instance sharedNumber :: Shared Number

instance cloneString :: Clone String where clone = unsafeClone
instance dropString :: Drop String where drop = unsafeDrop
instance sharedString :: Shared String

instance cloneArray :: (Shared a) => Clone (Array a) where clone = unsafeClone
instance dropArray :: (Shared a) => Drop (Array a) where drop = unsafeDrop
instance sharedArray :: (Shared a) => Shared (Array a)

instance cloneTuple :: (Clone a, Clone b) => Clone (Tuple a b) where
  clone = cloneTupleFFI Tuple fst snd clone clone
instance dropTuple :: (Drop a, Drop b) => Drop (Tuple a b) where
  drop = dropTupleFFI fst snd drop drop
instance sharedTuple :: (Shared a, Shared b) => Shared (Tuple a b)

foreign import cloneTupleFFI
  :: ∀ a b
   . (∀ l r. l -> r -> Tuple l r)
  -> (∀ l r. Tuple l r -> l)
  -> (∀ l r. Tuple l r -> r)
  -> (a -* Tuple a a)
  -> (b -* Tuple b b)
  -> Tuple a b
  -* Tuple (Tuple a b) (Tuple a b)

foreign import dropTupleFFI
  :: ∀ a b
   . (∀ l r. Tuple l r -> l)
  -> (∀ l r. Tuple l r -> r)
  -> (a -* Unit)
  -> (b -* Unit)
  -> Tuple a b
  -* Unit

--------------------------------------------------------------------------------

-- | A `Borrow` is a reference to a value. When a borrow to a value is live,
-- | the value cannot be accessed unless it is `Shared`. Some functions take
-- | borrows to avoid inconvenient tupling.
foreign import data Borrow :: Type -> Type

instance cloneBorrow :: Clone (Borrow a) where clone = unsafeClone
instance dropBorrow :: Drop (Borrow a) where drop = unsafeDrop
instance sharedBorrow :: (Shared a) => Shared (Borrow a)

-- | Borrow a value during the execution of a function.
borrow :: ∀ a b. Shared b => (Borrow a -* b) -> a -* Tuple a b
borrow = borrowFFI Tuple

foreign import borrowFFI
  :: ∀ a b
   . (∀ l r. l -> r -> Tuple l r)
  -> (Borrow a -* b)
  -> a
  -* Tuple a b
