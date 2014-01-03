--Cookbook.Ingredients.Lists.Modify
--Library for altering the contents of a list.

module Cookbook.Ingredients.Lists.Modify where

import qualified Cookbook.Essential.Continuous         as Cnt
import qualified Cookbook.Essential.Common             as Cm
import qualified Cookbook.Ingredients.Functional.Break as Br
import qualified Cookbook.Ingredients.Lists.Access     as Ac

rev :: [a] -> [a]
rev [] = []
rev (x:xs) = rev xs ++ [x]

rm :: (Eq a) => [a] -> a -> [a] -- See MDN1
rm x c = filter (/=c) x

splitOn :: (Eq a) => [a] -> a -> [[a]]
splitOn [] _ = []
splitOn x c = if (c `notElem` x) then [x] else (Cnt.before x c) : splitOn (Cnt.after x c) c

snipe :: (Eq a) => [a] -> (a,Int) -> [a]
snipe x (t,c) = (take c x) ++ [t] ++ (Cm.sub x (c + 1))

insert :: (Eq a) => [a] -> ([a],Int) -> [a]
insert x (t,c) = (take c x) ++ t ++ (Cm.sub x c)

between :: (Eq a) => [a] -> (a,a) -> [a]
between a (c,d) = Cnt.after (take (last $ Cm.positions a d) a) c

linesBetween :: (Eq a) => [[a]] -> ([a],[a]) -> [[a]]
linesBetween a (c,d) = tail $ Br.filterBreak (\e -> not $ Ac.contains e d) $ Br.removeBreak (\e -> not $ Ac.contains e c) a

intersperse :: [a] -> a -> [a]
intersperse [x] _ = [x]
intersperse (x:xs) c =  x:c : intersperse xs c

onLast :: (a -> a) -> [a] -> [a]
onLast _ []     = []
onLast f [x]    = f x : []
onLast f (x:xs) = x : onLast f xs

(?) :: (a -> b) -> (a -> b) -> Bool -> (a -> b)
(?) f1 f2 it = (if it then f1 else f2)


surroundedBy :: (Eq a) => [a] -> (a,a) -> [[a]] -- Map checks if a or b are not elememts.
surroundedBy x (a,b) = if or (map (not . (flip elem) x) [a,b]) then [] else recurse
  where recurse = Cnt.before (Cnt.after x a) b : surroundedBy (Cnt.after x b) (a,b)

-- [MDN1]
-- This is implemented in Continuous, but is kept here for historical purposes,
--    or for when Continuous is too heavy-duty for whatever job you are on.
