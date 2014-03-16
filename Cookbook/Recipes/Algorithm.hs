{- |
   Module      :   Cookbook.Recipes.Algorithm
   Copyright   :   (c) 2014 by Nate Pisarski
   License     :   BSD3
   Maintainer  :   nathanpisarski@gmail.com
   Stability   :   Stable
   Portability :   Portable (Cookbook)
Library for interacting with high-evel data structures found in Cookbook.Recipes.DataStructures (Ds). It also implements some data types that would have just been type synonyms if implemented in Ds.
-}

module Cookbook.Recipes.Algorithm where

import Cookbook.Recipes.DataStructures
import Cookbook.Ingredients.Lists.Modify

-- | Get every node of a tree, put it into a list.
climb :: (Tree a) -> [a]
climb x = case x of (Empty) -> [];(Branch a (Empty) b) -> a : climb b;
                    (Branch a b (Empty)) -> a : climb b;
                    (Branch a b d) -> a : climb b ++ climb d 

-- | Apply a function to every node in a tree.
treeMap :: (Tree a) -> (a -> a) -> (Tree a)
treeMap tr f = case tr of Empty -> Empty;
                          (Branch a (Empty) z@(Branch c _ _)) -> (Branch (f c) (treeMap z f) (Empty));
                          (Branch a z@(Branch b _ _) (Empty)) -> (Branch (f b) (Empty) (treeMap z f))

-- | Collectively nullify nodes on a tree.
treeFilter :: (Tree a) -> (a -> Bool) -> (Tree a)
treeFilter Empty f = Empty
treeFilter (Branch a (Empty) z@(Branch b _ _)) f = if (f a) then (Branch a (Empty) (treeFilter z f)) else Empty
treeFilter (Branch a z@(Branch b _ _) (Empty)) f = if (f a) then (Branch a (treeFilter z f) (Empty)) else Empty

-- | Generates a list of points, with the specified null data in the snd of the tupples.
genMatrix :: (Int,Int) -> a -> [((Int,Int),a)]
genMatrix (a,b) c = helper (0,0)
  where helper (d,e)
          | d == a = []
          | e == b = ((d,e),c) : helper (d+1,0)
          | otherwise = ((d,e),c) : helper (d,e+1)
