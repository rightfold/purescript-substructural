module Data.Function.Sub
  ( kind Capability
  , CLONE
  , DROP

  , class Clone, clone
  , class Drop, drop
  , unsafeClone
  , unsafeDrop
  , fst'
  , snd'

  , Sub
  , Linear
  , Affine
  , Relevant
  , type (-*)
  , type (-!)
  , type (-+)
  ) where

import Data.Tuple (Tuple(..), fst, snd)
import Prelude

--------------------------------------------------------------------------------

foreign import kind Capability

foreign import data CLONE :: Capability
foreign import data DROP :: Capability

--------------------------------------------------------------------------------

-- | Values which can be cloned. Instances of `Clone` must satisfy the
-- | following laws:
-- |
-- | - Clone: `fst' <<< clone = snd' <<< clone = id`
class Clone a where
  clone :: ∀ c. Sub (clone :: CLONE | c) a (Tuple a a)

-- | Values which can be dropped.
class Drop a where
  drop :: ∀ c. Sub (drop :: DROP | c) a Unit

-- | Unsafely clone a value.
unsafeClone :: ∀ c a. Sub (clone :: CLONE | c) a (Tuple a a)
unsafeClone = unsafeCloneFFI Tuple

-- | Unsafely drop a value.
foreign import unsafeDrop :: ∀ c a. Sub (drop :: DROP | c) a Unit

-- | Drop the second element of a tuple.
fst' :: ∀ c a b. Drop b => Sub (drop :: DROP | c) (Tuple a b) a
fst' = fst'FFI drop fst snd

-- | Drop the first element of a tuple.
snd' :: ∀ c a b. Drop a => Sub (drop :: DROP | c) (Tuple a b) b
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
  :: ∀ c a
   . (∀ l r. l -> r -> Tuple l r)
  -> Sub (clone :: CLONE | c) a (Tuple a a)

foreign import fst'FFI
  :: ∀ c a b
   . (b -! Unit)
  -> (∀ l r. Tuple l r -> l)
  -> (∀ l r. Tuple l r -> r)
  -> Sub (drop :: DROP | c) (Tuple a b) a

foreign import snd'FFI
  :: ∀ c a b
   . (a -! Unit)
  -> (∀ l r. Tuple l r -> l)
  -> (∀ l r. Tuple l r -> r)
  -> Sub (drop :: DROP | c) (Tuple a b) b

foreign import cloneArrayFFI
  :: ∀ c a
   . (a -+ Tuple a a)
  -> (∀ l r. l -> r -> Tuple l r)
  -> (∀ l r. Tuple l r -> l)
  -> (∀ l r. Tuple l r -> r)
  -> Sub (clone :: CLONE | c) (Array a) (Tuple (Array a) (Array a))

foreign import dropArrayFFI
  :: ∀ c a
   . (a -! Unit)
  -> Sub (drop :: DROP | c) (Array a) Unit

--------------------------------------------------------------------------------

-- | A function that is restricted to certain capabilities. The runtime
-- | representation is a unary JavaScript function that restricts itself to the
-- | stated capabilities.
foreign import data Sub :: # Capability -> Type -> Type -> Type

instance semigroupoidLinear :: Semigroupoid (Sub c) where
  compose = composeFFI

instance categoryLinear :: Category (Sub c) where
  id = idFFI

-- | You may not clone or drop the argument.
type Linear = Sub ()

-- | You may not clone the argument.
type Affine = Sub (drop :: DROP)

-- | You may not drop the argument.
type Relevant = Sub (clone :: CLONE)

infixr 4 type Linear as -*
infixr 4 type Affine as -!
infixr 4 type Relevant as -+

foreign import composeFFI :: ∀ c' a b c. Sub c' b c -> Sub c' a b -> Sub c' a c
foreign import idFFI :: ∀ c a. Sub c a a
