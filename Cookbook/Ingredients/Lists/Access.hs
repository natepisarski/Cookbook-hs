module Cookbook.Ingredients.Lists.Access(
count,contains,qsort,pull,refpos,areAll) where

import qualified Cookbook.Ingredients.Functional.Break as Br

import qualified Cookbook.Common as Cm

--Count the number of occurances in a list.
-- | Counts the number of occurences within a list.
count :: (Eq a) => [a] -> a -> Int
count x c = sum $ Br.btr (==c) x (1,0)

-- | Checks to see if a greater list has a list within it.
contains :: (Eq a) => [a] -> [a] -> Bool
contains [] _ = False
contains x c
  | (take (length c) x) == c = True
  | otherwise = contains (tail x) c 

-- | Sorts a list from least to greatest. Compose with Cookbook.Ingredients.Lists.Modify.rev for greatest-to-least.
qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) = lessT ++ [x] ++ greatT
  where
    lessT  = qsort [y | y <- xs, y <= x]
    greatT = qsort [y | y <- xs, y > x]

-- | Safe !!
pull :: [a] -> Int -> Maybe a
pull _ (-1) = Nothing
pull [] _  = Nothing
pull (x:xs) c = if c == 0 then (Just x) else pull xs (c - 1)

-- | Reference an element from one list to another.
refpos :: (Eq a) => ([a],[a]) -> a -> a
refpos (a,b) c = let d = (pull b (Cm.pos a c)) in case d of (Just x) -> x;(Nothing) -> c

-- | Are all elements of the list equal?
areAll :: (Eq a) => [a] -> a -> Bool
areAll x c = not $ Br.imbreak (/=c) x
