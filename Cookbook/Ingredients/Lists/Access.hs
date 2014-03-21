{- |
   Module      :   Cookbook.Ingredients.Lists.Access
   Copyright   :   (c) 2014 by Nate Pisarski
   License     :   BSD3
   Maintainer  :   nathanpisarski@gmail.com
   Stability   :   Stable
   Portability :   Portable (Cookbook)
Library for accessing the information from a list. Modify and Access are six in one and half-dozen in the other in a purely functional language, but the overall theme is this: Access is for functions which return portions of the list, or information about a list. Modify is the library which transforms a list.
-}
module Cookbook.Ingredients.Lists.Access where

import qualified Cookbook.Ingredients.Functional.Break as Br
import qualified Cookbook.Ingredients.Tupples.Assemble as As
import qualified Cookbook.Essential.Common             as Cm

-- | Counts the occurrences an element has within a list.
count :: (Eq a) => [a] -> a -> Int
count x c = sum $ Br.btr (==c)  (1,0) x

-- | Checks to see if a list is a sub-list of the list.
contains :: (Eq a) => [a] -> [a] -> Bool
contains [] _ = False
contains x c = (part x == c) || contains (tail x) c
  where part = take (length c)

-- | QuickSort implementation. Sorts a list of data quickly?
qsort :: (Ord a) => [a] -> [a] -- AKA Kenyan Sort
qsort [] = []
qsort (x:xs) = lessT ++ [x] ++ greatT
  where  (lessT,greatT)  = (qsort $ filter (<=x) xs,qsort $filter (>x) xs)

-- | Safe implementation of !!. Uses maybe instead of error.
pull :: [a] -> Int -> Maybe a
pull _ c | c < 0 = Nothing
pull [] _  = Nothing
pull (x:xs) c = if c == 0 then Just x else pull xs $ c - 1

-- | Referrential positioning. Find the position of an element in the first list, and return the element from the second list of the same position. In the event that the second list is shorter than the position where the element is found in the first list, it returns the parameter. 
refpos :: (Eq a) => ([a],[a]) -> a -> a
refpos (a,b) c = let d = pull b $ Cm.pos a c in case d of (Just x) -> x;(Nothing) -> c

-- | Test to make sure that all elements in a list are equal to a value.
areAll :: (Eq a) => [a] -> a -> Bool
areAll x c = not $ Br.imbreak (/=c) x

-- | Re-implementation of isPrefixOf for some reason. Tests to see if a list is the first part of antoher list.
isBefore :: (Eq a) => [a] -> [a] -> Bool
isBefore li tf = take (length tf) li == tf

-- | Test to see if a list is surrounded by an item.
surrounds :: (Eq a) => (a,a) -> [a] -> Bool
surrounds (b,c) a = (head a, last a) == (b,c)
