--Cookbook.Recipes.Algorithm
--Used for traversing data structures.
module Cookbook.Recipes.Algorithm(climb) where

import Cookbook.Recipes.DataStructures

climb :: (Tree a) -> a -> [a]
climb x c = case x of (Empty) -> [];
                      (Branch a (Empty) b) -> a : climb b c
                      (Branch a b (Empty)) -> a : climb b c
                      (Branch a b d) -> a : climb b c ++ climb d c
