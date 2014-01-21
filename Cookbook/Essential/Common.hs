{- |
   Module      :   Cookbook.Essential.Common
   Copyright   :   (c) 2014 by Nate Pisarski
   License     :   BSD3
   Maintainer  :   nathanpisarski@gmail.com
   Stability   :   Stable
   Portability :   Portable (Standalone - ghc)

"global" functions for the entirety of the Cookbook library.
Common is the potpourri of Cookbook, with no category except "everything uses me!"
-}
module Cookbook.Essential.Common where

-- | Return a list starting at an index. Indices start at 0.
sub :: (Eq a) => [a] -> Int -> [a]
sub [] _ = []
sub x 0 = x
sub (x:xs) c = sub xs (c - 1)

-- | Find the occurrences of an element in a list. 
positions :: (Eq a) => [a] -> a -> [Int]
positions x c = let y = zip x [0..(length x)] in find y
  where find y = [e | (d,e) <- y, d == c]

-- | C-style wrapper for positions. Returns the first occurrence in a list, or -1 on notElem.
pos :: (Eq a) => [a] -> a -> Int
pos x c | c `notElem` x = -1
pos x c = let ans = positions x c in ((if (length ans) > 1 then (head . tail) else head) ans)

-- | Reversal of map function. Chains calls of functions over a parameter, starting with head.
apply :: [(a -> a)] -> a -> a
apply [] c = c
apply (f:fs) c = apply fs (f c)

-- | Flatten a list one level.
flt :: [[a]] -> [a]
flt [] = []
flt (x:xs) = x ++ flt xs

-- | Execute a function from the end of a list to the front.
fromLast :: ([a] -> [a]) -> [a] -> [a]
fromLast f c = rev $ f $ rev c
  where rev [] = []
        rev (x:xs) = rev xs ++ [x]
