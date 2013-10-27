--Cookbook.Ingredients.Lists.Access
--Access is a library for generating statistics about a list. The lists are not changed as a result
--of executing any Access function.
module Cookbook.Ingredients.Lists.Access(count,contains,qsort,pull,refpos,areAll,isBefore,surrounds) where

import qualified Cookbook.Ingredients.Functional.Break as Br

import qualified Cookbook.Essential.Common as Cm

-- | Counts the number of occurrences within a list.
count :: (Eq a) => [a] -> a -> Int
count x c = sum $ Br.btr (==c)  (1,0) x

-- | Checks to see if a greater list has a list within it.
contains :: (Eq a) => [a] -> [a] -> Bool
contains [] _ = False
contains x c = if part x == c then True else contains (tail x) c
  where part = take (length c)

-- | Sorts a list from least to greatest. Compose with Cookbook.Ingredients.Lists.Modify.rev for greatest-to-least.
qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) = lessT ++ [x] ++ greatT
  where  (lessT,greatT)  = (qsort $ filter (<=x) xs,qsort $filter (>x) xs)

-- | Safer way than (!! (Prelude)) to get items out of a list. Returns nothing on failure.
pull :: [a] -> Int -> Maybe a
pull _ c | c < 0 = Nothing
pull [] _  = Nothing
pull (x:xs) c = if c == 0 then (Just x) else pull xs (c - 1)

-- | Reference an element from one list to another.
refpos :: (Eq a) => ([a],[a]) -> a -> a
refpos (a,b) c = let d = (pull b (Cm.pos a c)) in case d of (Just x) -> x;(Nothing) -> c

-- | Are all elements of the list equal?
areAll :: (Eq a) => [a] -> a -> Bool
areAll x c = not $ Br.imbreak (/=c) x

-- | Is the list a prefix of another?
isBefore :: (Eq a) => [a] -> [a] -> Bool
isBefore li tf = take (length tf) li == tf

surrounds :: (Eq a) => (a,a) -> [a] -> Bool
surrounds (b,c) a = (head a, last a) == (b,c)
