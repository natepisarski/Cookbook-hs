module Cookbook.Recipes.Stats(frequency,mostFrequent) where

import Cookbook.Ingredients.Tupples.Assemble
import Cookbook.Ingredients.Lists.Access
import Cookbook.Ingredients.Lists.Modify
import Cookbook.Recipes.Sanitize

-- | Return a list of all elements of the list with its frequency.
frequency :: (Eq a) => [a] -> [(a,Int)] 
frequency x = let y = map (\c -> (c,count x c)) x in rmdbAll y

-- | Get the x amount of most frequent items in the list.
mostFrequent :: (Eq a) => [a] -> Int -> [a]
mostFrequent x c = take c $ rev (assemble  $ frequency x)
