--Cookbook.Recipes.Diffstat
--A library for logging the movements of elements from one equally sized list to another, and patching others.
module Cookbook.Recipes.DiffStat(diff,patch) where

import qualified Cookbook.Essential.Common as  Cm
import qualified Cookbook.Ingredients.Tupples.Assemble as As

-- | Returns the transpositions each element of a list made.
diff :: (Eq a) => [a] -> [a] -> [Int]
diff  a b = map (\(c,d) -> d-c) difflists
  where difflists = zip [0..(length a)]  (map ( Cm.pos b) a) -- (orignal,new) positions

-- | Makes a list out of a difflist
patch :: (Eq a) => [a] -> [Int] -> [a]
patch x c = As.assemble $  zip x (map (\(d,e) -> d+e) ( zip c [0..(length x)]))
--Diff returns a relative list. patch has to add the index number of the list to the list to be meaningful in As.assemble
