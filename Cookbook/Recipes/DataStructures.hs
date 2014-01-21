{- |
   Module      :   Cookbook.Recipes.DataStructures
   Copyright   :   (c) 2014 by Nate Pisarski
   License     :   BSD3
   Maintainer  :   nathanpisarski@gmail.com
   Stability   :   Stable
   Portability :   Portable (Standalone - ghc)
Library for defining high-level generic data structures, most commonly containers. Specialized data types for specialized projects should go into Projects. It's sort of why it exists.
-}

module Cookbook.Recipes.DataStructures where

-- | Implementation of a binary tree.
data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show)--add a "map" and "filter" function for Tree.












