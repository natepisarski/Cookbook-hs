module Cookbook.Ingredients.Lists.Stats(frequency,mostFrequent) where

import qualified Cookbook.Ingredients.Tupples.Assemble as As
import qualified Cookbook.Ingredients.Lists.Access as Ac
import qualified Cookbook.Ingredients.Lists.Modify as Md
import qualified Cookbook.Recipes.Sanitize as Sn

-- | Return a list of all elements of the list with its frequency.
frequency :: (Eq a) => [a] -> [(a,Int)] 
frequency x = let y = map (\c -> (c,Ac.count x c)) x in Sn.rmdbAll y

-- | Get the x amount of most frequent items in the list.
mostFrequent :: (Eq a) => [a] -> Int -> [a]
mostFrequent x c = take c $ Md.rev (As.assemble  $ frequency x)

