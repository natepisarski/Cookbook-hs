--Cookbook.Ingredients.Lists.Access
--Access is a library for generating statistics about a list. The lists are not changed as a result
--of executing any Access function.

module Cookbook.Ingredients.Lists.Access where

import qualified Cookbook.Ingredients.Functional.Break as Br
import qualified Cookbook.Ingredients.Tupples.Assemble as As
import qualified Cookbook.Essential.Common             as Cm

count :: (Eq a) => [a] -> a -> Int
count x c = sum $ Br.btr (==c)  (1,0) x

contains :: (Eq a) => [a] -> [a] -> Bool
contains [] _ = False
contains x c = if part x == c then True else contains (tail x) c
  where part = take (length c)

qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) = lessT ++ [x] ++ greatT
  where  (lessT,greatT)  = (qsort $ filter (<=x) xs,qsort $filter (>x) xs)

pull :: [a] -> Int -> Maybe a
pull _ c | c < 0 = Nothing
pull [] _  = Nothing
pull (x:xs) c = if c == 0 then (Just x) else pull xs (c - 1)

refpos :: (Eq a) => ([a],[a]) -> a -> a
refpos (a,b) c = let d = (pull b (Cm.pos a c)) in case d of (Just x) -> x;(Nothing) -> c

areAll :: (Eq a) => [a] -> a -> Bool
areAll x c = not $ Br.imbreak (/=c) x

isBefore :: (Eq a) => [a] -> [a] -> Bool
isBefore li tf = take (length tf) li == tf

surrounds :: (Eq a) => (a,a) -> [a] -> Bool
surrounds (b,c) a = (head a, last a) == (b,c)
