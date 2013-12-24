--Cookbook.Ingredients.Lists.Modify
--Library for altering the contents of a list.

module Cookbook.Ingredients.Lists.Modify(rev,rm,splitOn,snipe,insert,between,linesBetween,surroundedBy) where

import qualified Cookbook.Essential.Continuous as Cnt
import qualified Cookbook.Essential.Common as Cm

import qualified Cookbook.Ingredients.Functional.Break as Br
import qualified Cookbook.Ingredients.Lists.Access as Ac

-- | Reverses a list
rev :: [a] -> [a]
rev [] = []
rev (x:xs) = rev xs ++ [x]

-- | Removes all occurences from a list.
rm :: (Eq a) => [a] -> a -> [a]
rm x c = filter (/=c) x

-- | Create sub-lists based on a delimeter. The string 'joe,joe1,joe2' splitOn ',' would return a list of the three joes.
splitOn :: (Eq a) => [a] -> a -> [[a]]
splitOn [] _ = []
splitOn x c = if (c `notElem` x) then [x] else (Cnt.before x c) : splitOn (Cnt.after x c) c

-- | Change a location in a list with an element.
snipe :: (Eq a) => [a] -> (a,Int) -> [a]
snipe x (t,c) = (take c x) ++ [t] ++ (Cm.sub x (c + 1))

-- | Snipe, workable with lists and does not delete information.
insert :: (Eq a) => [a] -> ([a],Int) -> [a]
insert x (t,c) = (take c x) ++ t ++ (Cm.sub x c)

-- | Find out what is in between two elements of a list
between :: (Eq a) => [a] -> (a,a) -> [a]
between a (c,d) = Cnt.after (take (last $ Cm.positions a d) a) c

-- | Selectively parse a list based on the existance of a substring.
linesBetween :: (Eq a) => [[a]] -> ([a],[a]) -> [[a]]
linesBetween a (c,d) = tail $ Br.filterBreak (\e -> not $ Ac.contains e d) $ Br.removeBreak (\e -> not $ Ac.contains e c) a

-- | Intersperse elements into a list.
intersperse :: [a] -> a -> [a]
intersperse [x] _ = [x]
intersperse (x:xs) c =  x:c : intersperse xs c

-- | Perform a function on the last element of a list.
onLast :: (a -> a) -> [a] -> [a]
onLast _ []     = []
onLast f [x]    = f x : []
onLast f (x:xs) = x : onLast f xs

(?) :: (a -> b) -> (a -> b) -> Bool -> (a -> b)
(?) f1 f2 it = (if it then f1 else f2)


surroundedBy :: (Eq a) => [a] -> (a,a) -> [[a]] -- Map checks if a or b are not elememts.
surroundedBy x (a,b) = if or (map (not . (flip elem) x) [a,b]) then [] else recurse
  where recurse = Cnt.before (Cnt.after x a) b : surroundedBy (Cnt.after x b) (a,b)
