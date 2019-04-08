{-# Language TypeFamilies #-}
{-# Language KindSignatures #-}
{-# Language AllowAmbiguousTypes #-}
module Data.Ropey.Classes where

import Data.FingerTree (Measured(..))
import Data.Ropey.Prelude
import qualified Prelude as P
import GHC.Stack
import Data.Monoid
import qualified Data.Text as T

class Monoid container => Container container element where
  {-# MINIMAL cons, snoc, uncons, unsnoc #-}
  cons :: element -> container -> container
  snoc :: container -> element -> container

  uncons :: container -> Maybe (element, container)
  unsnoc :: container -> Maybe (container, element)

  -- optional methods
  foldMapC ::  Monoid m => (element -> m) -> container -> m
  foldMapC f cnt = case uncons cnt of
      Just (element, container) -> f element <> foldMapC f container
      Nothing -> mempty

  unpack :: container -> [element]
  unpack c = case uncons c of
    Just (x, xs) -> x : unpack xs
    Nothing -> []

  pack :: [element] -> container
  pack = \case
    x:xs -> cons x $ pack xs
    [] -> mempty

packWithChunksOf ::
  (Indexed i elem, Container cont elem) =>
  i -> elem -> cont
packWithChunksOf i = pack . chunksOf i

instance Container T.Text Char where
  cons = T.cons
  snoc = T.snoc
  uncons = T.uncons
  unsnoc = T.unsnoc
  foldMapC f = T.foldr ((<>) . f) mempty
  unpack = T.unpack
  pack = T.pack

emptyList :: a
emptyList = error "Empty list"

head :: (Container container element, Partial)
     => container -> element
head c = case uncons c of
  Just (e,_) -> e
  Nothing -> emptyList

tail ::
  forall element container proxy.
  (Container container element, Partial) =>
  proxy element -> container -> container
tail _ c = case uncons c of
  Just ((_::element),c') -> c'
  Nothing -> emptyList

last ::
  (Container container element, Partial) =>
  container -> element
last c = case unsnoc c of
  Just (_,e) -> e
  Nothing -> emptyList

init ::
  forall element container proxy.
  (Container container element, Partial) =>
  proxy element -> container -> container
init _ c = case unsnoc c of
  Just (c',_::element) -> c'
  Nothing -> emptyList

all ::
  (Container container element) =>
  (element -> Bool) -> container -> Bool
all p = getAll . foldMapC (All . p)

any ::
  (Container container element) =>
  (element -> Bool) -> container -> Bool
any p = getAny . foldMapC (Any . p)

class Monoid container => Indexed index container where
  slices :: [index] -> container -> [container]
  length :: container -> index

instance Indexed Int T.Text where
  length = T.length
  slices = go
    where
      go (k:ks) t = case T.splitAt k t of
               (a,b) | T.null a    -> []
                     | otherwise -> a : go ks b
      go [] _ = []

splitAt ::
  Indexed index container =>
  index ->
  container ->
  (container, container)
splitAt i container = case slices [i] container of
  [r] -> (r, mempty)
  [take', drop'] -> (take', drop')
  xs -> error $
       "Slices returned a list of length "
    ++ show  (P.length xs)
    ++ " from a 1 length argument"

take ::
  Indexed index container =>
  index ->
  container ->
  container
take i = fst . splitAt i

drop ::
  Indexed index container =>
  index ->
  container ->
  container
drop i = snd . splitAt i

chunksOf ::
  Indexed index container =>
  index ->
  container ->
  [container]
chunksOf i = slices $ repeat i

type Partial = HasCallStack

-- | This allows you to write performant algorithms which break
--   data structures over
class Breakable container element where
  type BreakInfo container element :: *
  break :: BreakInfo container element -> (container, container)
  prepend :: element -> BreakInfo container element -> BreakInfo container element
  append :: BreakInfo container element -> element -> BreakInfo container element

-- | TODO make this container dependent
--   this is a number I picked randomly TODO benchmark
chunkSize :: Int
chunkSize = 65536
