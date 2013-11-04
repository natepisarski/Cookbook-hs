--Cookbook.Recipes.Algorithm
--Used for traversing data structures.
module Cookbook.Recipes.Algorithm(climb,genMatrix) where

import Cookbook.Recipes.DataStructures
import Cookbook.Ingredients.Lists.Modify

climb :: (Tree a) -> a -> [a]
climb x c = case x of (Empty) -> [];
                      (Branch a (Empty) b) -> a : climb b c
                      (Branch a b (Empty)) -> a : climb b c
                      (Branch a b d) -> a : climb b c ++ climb d c

genMatrix :: (Int,Int) -> a -> [((Int,Int),a)]
genMatrix (a,b) c = helper (0,0)
  where helper (d,e)
          | d == a = []
          | e == b = ((d,e),c) : helper (d+1,0)
          | otherwise = ((d,e),c) : helper (d,e+1)
