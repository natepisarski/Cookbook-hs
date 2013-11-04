--Cookbook.Recipes.Algorithm
--Used for traversing data structures.
module Cookbook.Recipes.Algorithm(climb,genMatrix) where

import Cookbook.Recipes.DataStructures
import Cookbook.Ingredients.Lists.Modify

-- | Gets all the nodes in a binary tree, putting them in a list.
climb :: (Tree a) -> [a]
climb x = case x of (Empty) -> [];(Branch a (Empty) b) -> a : climb b;
                    (Branch a b (Empty)) -> a : climb b;
                    (Branch a b d) -> a : climb b ++ climb d 

-- | Provides map-like behavior for binary trees.
treeMap :: (Tree a) -> (a -> a) -> (Tree a)
treeMap tr f = case tr of Empty -> Empty;
                          (Branch a (Empty) z@(Branch c _ _)) -> (Branch (f c) (treeMap z f) (Empty));
                          (Branch a z@(Branch b _ _) (Empty)) -> (Branch (f b) (Empty) (treeMap z f))

-- | Provides filter-like behavior for binary trees.
treeFilter :: (Tree a) -> (a -> Bool) -> (Tree a)
treeFilter Empty f = Empty
treeFilter (Branch a (Empty) z@(Branch b _ _)) f = if (f a) then (Branch a (Empty) (treeFilter z f)) else Empty
treeFilter (Branch a z@(Branch b _ _) (Empty)) f = if (f a) then (Branch a (treeFilter z f) (Empty)) else Empty

-- | Generates a matrix (coord1,coord2,a) with the specified data to null it out.
genMatrix :: (Int,Int) -> a -> [((Int,Int),a)]
genMatrix (a,b) c = helper (0,0)
  where helper (d,e)
          | d == a = []
          | e == b = ((d,e),c) : helper (d+1,0)
          | otherwise = ((d,e),c) : helper (d,e+1)
