-- | Bookkeeping is a method of keeping track of features of your String
--   for example, if you track book
module Data.Ropey.Bookkeeping where

import Data.Ropey.FingerTree (Measured(..))
import qualified Data.Text as T

data NoMeasure = NoMeasure

instance Semigroup NoMeasure where
  (<>) NoMeasure NoMeasure = NoMeasure

instance Monoid NoMeasure where
  mempty = NoMeasure

instance Measured NoMeasure container where
  measure = const NoMeasure

newtype Lines = Lines {unLines :: Int}
instance Semigroup Lines where
  Lines a <> Lines b = Lines $ a + b
instance Monoid Lines where
  mempty = Lines 0

newtype Length = Length {unLength :: Int}
instance Semigroup Length where
  Length a <> Length b = Length $ a + b
instance Monoid Length where
  mempty = Length 0

instance Measured Length T.Text where
  measure = Length . T.length

instance Measured Lines T.Text where
  measure = Lines . T.count "\n"

-- data M2 a b = M2 a b
-- data M3 a b c = M3 a b c

-- instance (Semigroup a, Semigroup b)
--   => Semigroup (M2 a b) where
--   M2 a1 b1 <>  M2 a2 b2 = uncurry M2 $ (a1,b1) <> (a2,b2)
-- instance (Monoid a, Monoid b)
--   => Monoid (M2 a b) where
--   mempty = uncurry M2 mempty

-- instance (Semigroup a, Semigroup b, Semigroup c)
--   => Semigroup (M3 a b c) where
--   M3 a1 b1 c1 <>  M3 a2 b2 c2 = uncurry3 M3 $ (a1,b1,c1) <> (a2,b2,c2)
-- instance (Monoid a, Monoid b, Monoid c)
--   => Monoid (M3 a b c) where
--   mempty = uncurry3 M3 mempty
