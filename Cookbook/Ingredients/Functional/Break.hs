--Cookbook.Ingredients.Functional.Break
--Break contains utilities for collectings lists conditionally. Unlike filter 
--from prelude, breaks will stop collecting elements of the list at the first
--untrue value. As an addition to this library are functions relating to
--mapping predicates over lists to yield either a different result or one solution.

module Cookbook.Ingredients.Functional.Break where

removeBreak :: (a -> Bool) -> [a] -> [a]
removeBreak _ [] = []
removeBreak f (c:cs) = if not $ f c then (c:cs) else removeBreak f cs

filterBreak :: (a -> Bool) -> [a] -> [a]
filterBreak _ [] = []
filterBreak f (c:cs) = if not $ f c then [] else c : filterBreak f cs

imbreak :: (a -> Bool) -> [a] -> Bool
imbreak f = or . map f

btr :: (a -> Bool) -> (b,b) -> [a] -> [b]
btr f (a,b) = map (\c -> if f c then a else b)

splitBool :: (Eq a) => (a -> Bool) -> [a] -> [[a]]
splitBool _ [] = []
splitBool f c = filter (/= []) (fP: if fP /= [] then (splitBool f tP) else [])
  where fP = filterBreak f c
        tP = removeBreak f c
