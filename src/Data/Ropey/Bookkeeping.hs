-- | Bookkeeping is a method of keeping track of features of your String
--   for example, if you track book
module Data.Ropey.Bookkeeping where

import Data.Ropey.FingerTree (Measured(..))

class Measured container measure => Bookkeeping container measure where

-- | This has an extra dependency because of a silly fundep in the FingerTree
--   package
data NoMeasure = NoMeasure

instance Semigroup NoMeasure where
  (<>) NoMeasure NoMeasure = NoMeasure

instance Monoid NoMeasure where
  mempty = NoMeasure

instance Measured NoMeasure container where
  measure = const NoMeasure
