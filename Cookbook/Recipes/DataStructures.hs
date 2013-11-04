--Cookbook.Recipes.DataStructures
--Higher-level data structures for use in the rest of Cookbook.
module Cookbook.Recipes.DataStructures(Tree(..),) where

-- | Cookie-cutter implementation of a binary tree.
data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show)--add a "map" and "filter" function for Tree.












