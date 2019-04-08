{-# Language GeneralizedNewtypeDeriving #-}
{-# Language FlexibleContexts #-}
{-# Language UndecidableInstances #-}
-- | This module is equivalent to Data.FingerTree except
--   <https://hub.darcs.net/ross/fingertree/issue/1 issue 1> has been fixed
--   the performance is identical and converting between the data types is free
--   mostly copied directly from "Data.FingerTree"
module Data.Ropey.FingerTree (
    FingerTree,
    Measured(..),
    -- * Construction
    empty, singleton,
    (<|), (|>), (><),
    fromList,
    -- * Deconstruction
    null,
    -- ** Examining the ends
    FT.ViewL(..), viewl,
    FT.ViewR(..), viewr,
    -- ** Search
    FT.SearchResult(..), search,
    -- ** Splitting
    -- | These functions are special cases of 'search'.
    split, takeUntil, dropUntil,
    -- * Transformation
    reverse,
    -- ** Maps
    fmap', fmapWithPos, fmapWithContext, unsafeFmap,
    -- ** Traversals
    traverse', traverseWithPos, traverseWithContext, unsafeTraverse,
    -- ** Conversion functions
    toFT, fromFT,
  ) where

import qualified Prelude as P
import Prelude hiding (null, reverse)
import qualified Data.FingerTree as FT
import Data.Ropey.Classes as Cl
import Unsafe.Coerce
import Data.Coerce
import qualified Data.List.Extra as List
import GHC.Generics
import Data.Foldable hiding (null)
import Control.Applicative hiding (empty)

newtype FingerTree v a = FT {unFT :: FT.FingerTree v (M v a)}
  deriving (Semigroup, Monoid, Eq, Ord, Show, Generic)

instance Foldable (FingerTree v) where
  foldMap p = foldMap (p . unM) . unFT

instance {-# OVERLAPS #-} Measured v a => Cl.Container (FingerTree v a) a where
  cons = (<|)
  snoc = (|>)
  uncons c = case viewl c of
    e FT.:< _ -> Just (e,c)
    FT.EmptyL -> Nothing
  unsnoc c = case viewr c of
    _ FT.:> e -> Just (c,e)
    FT.EmptyR -> Nothing
  foldMapC = foldMap
  unpack = toList
  pack = fromList


-- | TODO be smart about chunk sizes
-- This could be more generic, but it will make instances less predictable
-- TODO add an indexed constraint
instance forall a b v. (Measured v a, Cl.Container a b) => Cl.Container (FingerTree v a) b where
  -- | TODO make less inefficient
  cons e = cons (pack [e] :: a)

  -- | TODO make less inefficient
  snoc c e = snoc c (pack [e] :: a)

  uncons :: FingerTree v a -> Maybe (b, FingerTree v a)
  uncons c = do
    (e :: a, c') <- uncons c
    let wrk (b, a) = (b, cons a c')
    (wrk <$> uncons e) <|> uncons c'

  unsnoc :: FingerTree v a -> Maybe (FingerTree v a, b)
  unsnoc c = do
    (c', e :: a) <- unsnoc c
    let wrk (a,b) = (snoc c' a, b)
    (wrk <$> unsnoc e) <|> unsnoc c'

  foldMapC :: forall m. Monoid m => (b -> m) -> FingerTree v a -> m
  foldMapC f = foldMapC (foldMapC f :: a -> m)

  unpack :: FingerTree v a -> [b]
  unpack ft = do
    (a :: a) <- unpack ft
    unpack a

  pack :: [b] -> FingerTree v a
  pack =
    (pack :: [a] -> FingerTree v a) .
    map (pack :: [b] -> a) .
    List.chunksOf chunkSize

-- | This type is used to satisfy the functional dependency in `Measured`
newtype M measure container = M {unM :: container}
  deriving (Eq, Ord, Show, Generic, Semigroup, Monoid)

instance Functor (M measure) where
  fmap f (M x) = M $ f x

instance Applicative (M measure) where
  pure = M
  (M f) <*> (M x) = M $ f x

class P.Monoid measure => Measured measure container where
  measure :: container -> measure

instance Measured measure container
      => FT.Measured measure (M measure container) where
  measure (M container) = measure container
  -- not recursive due to funny haskell qualification rules

instance Measured measure container
      => Measured measure (FingerTree measure container) where
  measure = foldMap measure

-- | /O(1)/. The empty sequence.
empty :: Measured v a => FingerTree v a
empty = FT FT.empty

-- | /O(1)/. A singleton sequence.
singleton :: Measured v a => a -> FingerTree v a
singleton = FT . FT.singleton . M

-- | /O(1)/. Add an element to the left end of a sequence.
-- Mnemonic: a triangle with the single element at the pointy end.
(<|) :: Measured v a => a -> FingerTree v a -> FingerTree v a
(<|) a ft = FT $ M a FT.<| unFT ft

-- | /O(1)/. Add an element to the right end of a sequence.
-- Mnemonic: a triangle with the single element at the pointy end.
(|>) :: Measured v a => FingerTree v a -> a -> FingerTree v a
(|>) ft a = FT $ unFT ft FT.|> M a

-- | /O(log(min(n1,n2)))/. Concatenate two sequences.
(><) :: Measured v a => FingerTree v a -> FingerTree v a -> FingerTree v a
(><) a b = FT $ unFT a FT.>< unFT b

-- | /O(n)/. Create a sequence from a finite list of elements.
-- The opposite operation 'toList' is supplied by the 'Foldable' instance.
fromList :: Measured v a => [a] -> FingerTree v a
fromList = FT . FT.fromList . map M

-- | /O(1)/. Is this the empty sequence?
null :: FingerTree v a -> Bool
null = FT.null . unFT

-- | /O(1)/. Analyse the left end of a sequence.
viewl :: Measured v a => FingerTree v a -> FT.ViewL (FingerTree v) a
viewl = c . FT.viewl . unFT where
  -- | I hope this gets optimised away, I suspect GHC can remove "inspect the
  --   structure then always return the same structure" at runtime
  _c, c :: FT.ViewL (FT.FingerTree v) (M v a) ->
       FT.ViewL (FingerTree v) a
  c = unsafeCoerce
  -- | _c is a witness that unsafeCoerce isn't so unsafe
  _c = \case
    FT.EmptyL -> FT.EmptyL
    (M v) FT.:< ft -> v FT.:< FT ft

-- | /O(1)/. Analyse the right end of a sequence.
viewr :: Measured v a => FingerTree v a -> FT.ViewR (FingerTree v) a
viewr = c . FT.viewr . unFT where
  -- | same as above
  _c, c :: FT.ViewR (FT.FingerTree v) (M v a) ->
           FT.ViewR (FingerTree v) a
  c = unsafeCoerce
  _c = \case
    FT.EmptyR -> FT.EmptyR
    ft FT.:> (M v) -> FT ft FT.:> v

search :: Measured v a =>
          (v -> v -> Bool) ->
          FingerTree v a ->
          FT.SearchResult v a
search p = c
         . (FT.search p)
         . (unFT) where
  -- | same as above
  _c, c :: FT.SearchResult v (M v a) ->
           FT.SearchResult v a
  c = unsafeCoerce
  _c = \case
    FT.Position ftA (M v) ftB -> FT.Position (FT.unsafeFmap coerce ftA) v (FT.unsafeFmap coerce ftB)
    FT.OnLeft -> FT.OnLeft
    FT.OnRight -> FT.OnRight
    FT.Nowhere -> FT.Nowhere

-- For predictable results, one should ensure that there is only one such
-- point, i.e. that the predicate is /monotonic/.
split ::  (Measured v a) =>
          (v -> Bool) ->
          FingerTree v a ->
          (FingerTree v a, FingerTree v a)
split p ft = (FT l, FT r) where
  (l,r) = FT.split p $ unFT ft

-- | /O(log(min(i,n-i)))/.
-- Given a monotonic predicate @p@, @'takeUntil' p t@ is the largest
-- prefix of @t@ whose measure does not satisfy @p@.
--
-- *  @'takeUntil' p t = 'fst' ('split' p t)@
takeUntil :: Measured v a => (v -> Bool) -> FingerTree v a -> FingerTree v a
takeUntil p = FT . FT.takeUntil p . unFT

-- | /O(log(min(i,n-i)))/.
-- Given a monotonic predicate @p@, @'dropUntil' p t@ is the rest of @t@
-- after removing the largest prefix whose measure does not satisfy @p@.
--
-- * @'dropUntil' p t = 'snd' ('split' p t)@
dropUntil :: Measured v a => (v -> Bool) -> FingerTree v a -> FingerTree v a
dropUntil p = FT . FT.dropUntil p . unFT

-- | /O(n)/. The reverse of a sequence.
reverse :: Measured v a => FingerTree v a -> FingerTree v a
reverse = FT . FT.reverse . unFT

-- | Like 'fmap', but with constraints on the element types.
fmap' :: (Measured v1 a1, Measured v2 a2) =>
         (a1 -> a2) ->
         FingerTree v1 a1 ->
         FingerTree v2 a2
fmap' f = FT . FT.fmap' (coerce . fmap f) . unFT

-- | Map all elements of the tree with a function that also takes the
-- measure of the prefix of the tree to the left of the element.
fmapWithPos :: (Measured v1 a1, Measured v2 a2) =>
               (v1 -> a1 -> a2) -> FingerTree v1 a1 -> FingerTree v2 a2
fmapWithPos f = FT . FT.fmapWithPos (\x -> coerce . fmap (f x)) . unFT

-- | Map all elements of the tree with a function that also takes the
-- measure of the prefix to the left and of the suffix to the right of
-- the element.
--
-- @since 0.1.2.0
fmapWithContext :: (Measured v1 a1, Measured v2 a2) =>
    (v1 -> a1 -> v1 -> a2) -> FingerTree v1 a1 -> FingerTree v2 a2
fmapWithContext f =
    FT
  . FT.fmapWithContext (\v1 (M a1) v2 -> M $ f v1 a1 v2)
  . unFT

-- | Like 'fmap', but safe only if the function preserves the measure.
unsafeFmap :: (a -> b) -> FingerTree v a -> FingerTree v b
unsafeFmap f = FT . FT.unsafeFmap (fmap f) . unFT

-- | Like 'traverse', but with constraints on the element types.
traverse' :: (Measured v1 a1, Measured v2 a2, Applicative f) =>
    (a1 -> f a2) -> FingerTree v1 a1 -> f (FingerTree v2 a2)
traverse' f = fmap FT . FT.traverse' (\(M a) -> M <$> f a) . unFT

-- | Traverse the tree from left to right with a function that also
-- takes the measure of the prefix of the tree to the left of the element.
traverseWithPos :: (Measured v1 a1, Measured v2 a2, Applicative f) =>
    (v1 -> a1 -> f a2) -> FingerTree v1 a1 -> f (FingerTree v2 a2)
traverseWithPos f = fmap FT
                  . FT.traverseWithPos (\x (M a) -> M <$> f x a)
                  . unFT

-- | Traverse the tree from left to right with a function that also
-- takes the measure of the prefix to the left and the measure of the
-- suffix to the right of the element.
--
-- @since 0.1.2.0
traverseWithContext :: (Measured v1 a1, Measured v2 a2, Applicative f) =>
    (v1 -> a1 -> v1 -> f a2) -> FingerTree v1 a1 -> f (FingerTree v2 a2)
traverseWithContext f =
    fmap FT
  . FT.traverseWithContext (\v1 (M a) v2 -> M <$> f v1 a v2)
  . unFT

-- | Like 'traverse', but safe only if the function preserves the measure.
unsafeTraverse :: (Applicative f) =>
    (a -> f b) -> FingerTree v a -> f (FingerTree v b)
unsafeTraverse f = fmap FT . FT.unsafeTraverse (\(M a) -> M <$> f a) . unFT

-- | takes you from this modules FingerTree to the normal one
--   this has no runtime cost
toFT :: FingerTree v a -> FT.FingerTree v a
toFT = unsafeCoerce

-- | takes you from the normal Fingertree to this modules one
--   this has no runtime cost
fromFT :: FT.FingerTree v a -> FingerTree v a
fromFT = unsafeCoerce
