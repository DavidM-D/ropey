-- | Like a lot of data type libraries we export names which clash with the
--   stdlib, this exports the prelude hiding the names we use internally
module Data.Ropey.Prelude (module P) where

import Prelude as P hiding
       (length, null, length, splitAt, break, span, all, any, reverse
       , foldl, foldr, head, tail, writeFile, readFile, Word, last, init
       , concat, take, replicate
       )
