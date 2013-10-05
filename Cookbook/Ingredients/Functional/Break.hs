--Cookbook.Ingredients.Functional.Break
--Break contains utilities for collectings lists conditionally. Unlike filter 
--from prelude, breaks will stop collecting elements of the list at the first
--untrue value. As an addition to this library are functions relating to
--mapping predicates over lists to yield either a different result or one solution.

module Cookbook.Ingredients.Functional.Break(removeBreak, filterBreak,btr,imbreak) where

-- | When the predicate returns false, removeBreak returns the rest of the list.
removeBreak :: (a -> Bool) -> [a] -> [a]
removeBreak _ [] = []
removeBreak f (c:cs) = if not $ f c then (c:cs) else removeBreak f cs

-- | When the predicate returns false, filterBreak will stop collecting the list.
filterBreak :: (a -> Bool) -> [a] -> [a]
filterBreak _ [] = []
filterBreak f (c:cs) = if not $ f c then [] else c : filterBreak f cs

-- | imbreak will return true if any of the members of the list satisfy the predicate.
imbreak :: (a -> Bool) -> [a] -> Bool
imbreak f = or . map f

-- | Conditionally transform input based on the predicate. When it is true, fst of the tupple is used, snd otherwise.
btr :: (a -> Bool) -> (b,b) -> [a] -> [b]
btr f (a,b) = map (\c -> if f c then a else b)
