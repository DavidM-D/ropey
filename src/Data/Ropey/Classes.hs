module Data.Ropey.Classes where


import Data.Ropey.Prelude
import qualified Prelude as P

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
    (x:xs) -> cons x $ pack xs
    [] -> mempty


-- | Indexed laws
--   let l = deIndex (id @index) $ length r
--   in length (unpack (take (Index @index n) r) :: [t]) == max l n
--   splitAt n r == (take n r, drop n r)
--   splitAtEnd n r == (takeEnd n r, dropEnd n r)
--   splitAtEnd n == splitAt n . reverse
class (Monoid container, Num index) => Indexed index container where
  -- | slices of size n where n >= 0
  slices :: [index] -> container -> [container]
  length :: container -> index

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
