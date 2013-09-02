--Library for determining movement of data within a list. 
--data Stat can have one of 3 forms:
--Less a = Back by a
--Even  = Same position
--Great a = Forward by a
module Cookbook.Recipes.Diffstat(Stat(..),stat,diff,patch) where

import Cookbook.Common
import Cookbook.Ingredients.Lists.Modify
import Cookbook.Ingredients.Lists.Access
import Cookbook.Ingredients.Tupples.Look

data Stat a = Less a | Even |  Great a deriving (Show)

stat :: (Eq a) => [a] -> [a] -> a -> Stat Int
stat x c f
  | f `notElem` x = error "Unique entry found in stat x"
  | f `notElem` c = error "Unique entry found in stat c"
stat x c f
  | diff < 0  = Less diff
  | diff == 0 = Even
  | otherwise = Great diff
  where diff = (pos x f) - (pos c f)

diff :: (Eq a) => [a] -> [a] -> [Stat Int]
diff x [] = []
diff x (c:cs) = stat x (c:cs) c : diff x cs

--The compiler doesn't like this type signature for some reason.
--Here it is anyway, and :t in GHCI can even back it up:
--patch :: (Eq a) => [a] -> [Stat Int] -> [Int] 
patch x c = assemble $ relatdiff posbind c
  where posbind = zip x [0..((length x))]
        
--Get the absolute positions of diff lists.
relatdiff :: (Eq a) => [(a,Int)] -> [Stat Int] -> [(a,Int)]
relatdiff [] _ = []
relatdiff _ [] = []
relatdiff ((a,b):bs) ((Great x):xs) = (a,(b + x)) : relatdiff bs xs
relatdiff ((a,b):bs) ((Less x):xs) = (a,(b - x)) : relatdiff bs xs
relatdiff ((a,b):bs) ((Even):xs) = (a,(b-1)) : relatdiff bs xs

--Assemble a relatdiff list
assemble :: [(a,Int)] -> [a]
assemble x = map (look (map swp x))  $ qsort (map snd x)
