{-# Language GeneralizedNewtypeDeriving #-}
module Data.Ropey.Generic where

import Data.Ropey.Classes
import Data.Monoid

import Prelude hiding
       (length, null, length, splitAt, break, span, all, any, reverse
       , foldl, foldr, head, tail, writeFile, readFile, Word, last, init
       , concat, take, replicate
       )
import qualified Prelude as P


newtype Index a = I Int
  deriving (Show, Eq, Ord, Num)

-- | this allows you to operate on indexed containers from their end
--   take (FromEnd 2) "hello world" == "ld"
--   slices (map FromEnd [2,4,100]) "hello world" ["ld", "or", "hello w"]
newtype FromEnd a = FromEnd {unFromEnd :: a}
  deriving (Show, Eq, Ord, Num)

instance forall index container.
       ( Indexed index container
       , Num index ) => -- I don't like this Num constraint
         Indexed (FromEnd index) container where
  length = FromEnd . length
  slices is c = P.reverse $ slices indicies c where
    (l :: index) = length c
    (indicies :: [index]) = P.reverse $ map ((l -) . unFromEnd) is

any :: Container container element
    => (element -> Bool)
    -> container
    -> Bool
any f = getAny
      . foldMapC (Any . f)

all :: Container container element
    => (element -> Bool)
    -> container
    -> Bool
all f = getAll
      . foldMapC (All . f)

singleton ::
  Container container element =>
  element -> container
singleton e = pack [e]

tail ::
  forall element container proxy.
  Container container element =>
  proxy element -> container -> container
tail _ c = maybe mempty snd (uncons c :: Maybe (element, container))
-- this could also be implemented from indexed

head ::
  forall container element.
  (Partial, Container container element) =>
  container -> element
head c = maybe (error "'head' of an empty container") fst (uncons c :: Maybe (element, container))

init ::
  forall element container proxy.
  Container container element =>
  proxy element -> container -> container
init _ c = maybe mempty fst (unsnoc c :: Maybe (container, element))

last ::
  forall container element.
  (Partial, Container container element) =>
  container -> element
last c = maybe (error "'last' of an empty container") snd (unsnoc c :: Maybe (container, element))

null ::
  (Eq container, Monoid container) =>
  container -> Bool
null = (==) mempty

replicate ::
  Container container element =>
  Int -> element -> container
replicate i = pack . P.replicate i

span ::
  forall container element.
  Container container element =>
  (element -> Bool) ->
  container ->
  (container, container)
span p c = case uncons c :: Maybe (element, container) of
  Just (e, d) ->
    if p e
    then (\(t,f) -> (snoc t e, f)) $ span p d
    else (mempty, c)
  Nothing -> (mempty, mempty)
