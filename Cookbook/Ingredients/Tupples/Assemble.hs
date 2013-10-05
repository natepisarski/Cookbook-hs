--Cookbook.Ingredients.Tupples.Assemble
--Assemble is a library for sorting and constructing lists from touples with a member of Ord in the second slot.
--It is used for frequency analyzation, as well as other very particular tasks, but is kept out of Recipes because it is still generic.
module Cookbook.Ingredients.Tupples.Assemble(tupsort,assemble) where

-- | Quicksort a list of tupples, with the (Ord) element being the second one.
tupsort :: (Ord b) => [(a,b)] -> [(a,b)]
tupsort [] = []
tupsort ((a,n):xs) = lesser ++ [(a,n)] ++ greater
  where lesser   = tupsort [(c,d) | (c,d) <- xs, d <= n]
        greater = tupsort [(c,d) | (c,d) <- xs, d > n]

-- | Orders element-Ord tupples and gets all of the first elements.
assemble :: (Ord b) => [(a,b)] -> [a]
assemble = (map fst) . tupsort
