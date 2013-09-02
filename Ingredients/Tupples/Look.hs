module Cookbook.Ingredients.Tupples.Look(look,swp) where

look :: (Eq a) => [(a,b)] -> a -> b
look [] _ = error "Not found within tupple; Cookbook.Ingredients.Tupples 4:0"
look ((a,b):bs) c
  | a == c = b
  | otherwise = look bs c
  
swp :: (a,b) -> (b,a)
swp (a,b) = (b,a)
