{- |
   Module      :   Cookbook.Ingredients.Functional.Break
   Copyright   :   (c) 2014 by Nate Pisarski
   License     :   BSD3
   Maintainer  :   nathanpisarski@gmail.com
   Stability   :   Stable
   Portability :   Portable (Standalone - ghc)

Library for breaking conditionally on lists. When one filters a list, it will filter all of the elements. When one filterBREAKS a list, it will stop collecting the list at a desired element. This library also includes functions for conditionally transforming a list.
-} -- I Always imagine this library being really mad. FilterBREAK SMASH! RAAAAGH

module Cookbook.Ingredients.Functional.Break where

-- | Drop a list until a predicate yields false, returning the false item and the rest of the list.
removeBreak :: (a -> Bool) -> [a] -> [a]
removeBreak _ [] = []
removeBreak f (c:cs) = if not $ f c then c:cs else removeBreak f cs

-- | Collect a list until a predicate yields false for a value.
filterBreak :: (a -> Bool) -> [a] -> [a]
filterBreak _ [] = []
filterBreak f (c:cs) = if not $ f c then [] else c : filterBreak f cs

-- | Returns true if any element in the list yields true for a predicate.
imbreak :: (a -> Bool) -> [a] -> Bool
imbreak = any

-- | Conditionally transform a list. If a predicate returns true, use lval. Otherwise, use rval.
btr :: (a -> Bool) -> (b,b) -> [a] -> [b]
btr f (a,b) = map (\c -> if f c then a else b)
