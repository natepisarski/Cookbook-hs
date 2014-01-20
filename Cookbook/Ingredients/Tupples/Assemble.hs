module Cookbook.Ingredients.Tupples.Assemble where

tupsort :: (Ord b) => [(a,b)] -> [(a,b)]
tupsort [] = []
tupsort ((a,n):xs) = lesser ++ [(a,n)] ++ greater
  where lesser   = tupsort [(c,d) | (c,d) <- xs, d <= n]
        greater = tupsort [(c,d) | (c,d) <- xs, d > n]

assemble :: (Ord b) => [(a,b)] -> [a]
assemble = (map fst) . tupsort

rmDb :: (Eq a) =>[(a,b)] -> [(a,b)]
rmDb [] = []
rmDb ((a,b):c) = (a,b) : rmDb [(d,e) | (d,e) <- c, d /= a]
