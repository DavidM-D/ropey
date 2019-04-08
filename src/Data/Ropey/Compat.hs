{-# Language GeneralizedNewtypeDeriving #-}
{-# Language TypeApplications #-}
{-# Language StandaloneDeriving #-}
{-# Language FlexibleContexts #-}
module Data.Ropey.Compat (
    -- * Size
      YiString
    , length    -- :: Rope -> Int
    , null      -- :: Rope -> Bool
    , reverse   -- :: Rope -> Rope
    , replicate -- :: Int -> Rope -> Rope
    -- * Slicing
    , Breakable(..)
    , splitAt   -- :: Int -> Rope -> (Rope, Rope)
    , take      -- :: Int -> Rope -> Rope
    , drop      -- :: Int -> Rope -> Rope

    , head
    , tail
    , init
    , last
    , all
    , any
    -- * Walking
    -- ** construction
    , Indexed(..)
    , splitAtLine

    , Packable(..)
    , fromText           -- :: Text -> Rope
    , fromChunks         -- :: [Text] -> Rope
    , fromLazyText       -- :: L.Text -> Rope
    , fromChar           -- :: Char -> Rope
    , fromString         -- :: String -> Rope
    -- * Deconstructing 'Rope's
    , Unpackable(..)
    , toChunks           -- :: Rope -> [Text]
    , toLazyText         -- :: Rope -> L.Text
    , toString           -- :: Rope -> String
    , toText
    , ropeL
    , chunkText

    , concat
    , append
    ) where

import qualified Prelude as P
import Prelude hiding
  ( concat, length, null, reverse, replicate, splitAt, take, drop, head, tail, init, last
  , all, any, splitAtLine, fromText, fromChunks, fromLazyText, fromChar, fromString, toChunks
  , toLazyText, toString, toText, ropeL, chunkText)
import Data.Monoid
import Control.Exception
import qualified Data.Text as T
import Data.Ropey.Bookkeeping
import Data.Ropey.FingerTree as FT
import qualified Data.Ropey.Classes as Cl
import Data.Proxy
import qualified Data.List.Extra as List
import GHC.Generics

data Info = Info {iLength :: !Length, iLines :: !Lines}

instance Semigroup Info where
  Info len1 lin1 <> Info len2 lin2 = Info (len1 <> len2) (lin1 <> lin2)

instance Monoid Info where
  mempty = Info mempty mempty

instance Measured Info T.Text where
  measure t = Info (measure t) (measure t)

newtype YiString = YiString { fromRope :: FingerTree Info T.Text }
  deriving (Semigroup, Monoid, Generic)

fromString :: String -> YiString
fromString = fromString' Cl.chunkSize

fromText :: T.Text -> YiString
fromText = fromText' Cl.chunkSize

fromString' :: Int -> String -> YiString
fromString' i = YiString . Cl.pack . map T.pack . List.chunksOf i

fromText' :: Int -> T.Text -> YiString
fromText' i = YiString . Cl.packWithChunksOf i

toString :: YiString -> String
toString = P.concat . map T.unpack . Cl.unpack . fromRope

toReverseString :: YiString -> String
toReverseString = P.reverse . toString

toText :: YiString -> T.Text
toText = T.concat . Cl.unpack . fromRope

toReverseText :: YiString -> T.Text
toReverseText = T.reverse . toText

null :: YiString -> Bool
null = FT.null . fromRope

empty :: YiString
empty = mempty

take :: Int -> YiString -> YiString
take = undefined

drop :: Int -> YiString -> YiString
drop  = undefined

length :: YiString -> Int
length = unLength . iLength . measure . fromRope

reverse :: YiString -> YiString
reverse = fromText . T.reverse . toText

countNewLines :: YiString -> Int
countNewLines = unLines . iLines . measure . fromRope

lines :: YiString -> [YiString]
lines = undefined

lines' :: YiString -> [YiString]
lines' = undefined

unlines :: [YiString] -> YiString
unlines = undefined

splitAt, splitAtLine :: Int -> YiString -> (YiString, YiString)
(splitAt, splitAtLine) = (splitAtG . Length, splitAtG . Lines)
  where splitAtG ::
          Cl.Indexed i (FingerTree Info T.Text) =>
          i -> YiString -> (YiString, YiString)
        splitAtG i =
          (\(a,b) -> (YiString a, YiString b)) .
          Cl.splitAt i .
          fromRope

head :: YiString -> Char
head = Cl.head . fromRope

last :: YiString -> Char
last = Cl.last . fromRope

append :: YiString -> YiString -> YiString
append = (<>)

concat :: [YiString] -> YiString
concat = mconcat


-- | O(n) do all characters in the string satisfy the predicate?
all :: (Char -> Bool) -> YiString -> Bool
all p = Cl.all p . fromRope

-- | O(n) do any characters in the string satisfy the predicate?
any :: (Char -> Bool) -> YiString -> Bool
any p = Cl.any p . fromRope


tail :: YiString -> YiString
tail = YiString . Cl.tail (Proxy @Char) . fromRope

init :: YiString -> YiString
init = YiString . Cl.init (Proxy @Char) . fromRope

span :: (Char -> Bool) -> YiString -> (YiString, YiString)
span = undefined

break :: (Char -> Bool) -> YiString -> (YiString, YiString)
break = undefined

foldl' :: (a -> Char -> a) -> a -> YiString -> a
foldl' = undefined

replicate :: Int -> YiString -> YiString
replicate = undefined

replicateChar :: Int -> Char -> YiString
replicateChar i = fromString . P.replicate i
