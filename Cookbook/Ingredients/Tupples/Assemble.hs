module Cookbook.Ingredients.Tupples.Assemble(tupsort,assemble) where


tupsort :: (Ord b) => [(a,b)] -> [(a,b)]
tupsort [] = []
tupsort ((a,n):xs) = lesser ++ [(a,n)] ++ greater
  where lesser   = tupsort [(c,d) | (c,d) <- xs, d <= n]
        greater = tupsort [(c,d) | (c,d) <- xs, d > n]

assemble :: (Ord b) => [(a,b)] -> [a]
assemble = (map fst) . tupsort
