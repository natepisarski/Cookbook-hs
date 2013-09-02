module Cookbook.Ingredients.Lists.Access(
count,contains,qsort) where

import qualified Cookbook.Ingredients.Functional.Break as Br

import qualified Cookbook.Common as Cm

--Count the number of occurances in a list.
count :: (Eq a) => [a] -> a -> Int
count x c = sum $ Br.btr (==c) x (1,0)

contains :: (Eq a) => [a] -> [a] -> Bool
contains [] _ = False
contains x c
  | (take (length c) x) == c = True
  | otherwise = contains (tail x) c 

qsort :: (Num a, Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) = lessT ++ [x] ++ greatT
  where
    lessT  = qsort [y | y <- xs, y <= x]
    greatT = qsort [y | y <- xs, y > x]
