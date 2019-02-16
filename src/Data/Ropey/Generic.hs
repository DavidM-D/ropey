{-# Language GeneralizedNewtypeDeriving #-}
module Data.Ropey.Generic where

import Data.Ropey.Classes

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
