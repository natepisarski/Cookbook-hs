{-# LANGUAGE FlexibleContexts #-}
module Cookbook.Ingredients.Lists.Modify(
rev,rm,splitOn,snipe) where

import qualified Cookbook.Continuous as Cnt
import qualified Cookbook.Common as Com
--Reverse a list
--rev [1,2,3,4] -> [4,3,2,1]
-- | Reverses a list
rev :: [a] -> [a]
rev [] = []
rev (x:xs) = rev xs ++ [x]

-- | Removes all occurences from a list.
rm :: (Eq a) => [a] -> a -> [a]
rm x c = filter (/=c) x

-- | Create sub-lists based on a delimeter. The string 'joe,joe1,joe2' splitOn ',' would return a list of the three joes.
splitOn :: (Cnt.Continuous [a] a, Eq a) => [a] -> a -> [[a]]
splitOn [] _ = []
splitOn x c = if (c `notElem` x) then [x] else (Cnt.before x c) : splitOn (Cnt.after x c) c

snipe :: (Eq a) => [a] -> (a,Int) -> [a]
snipe x (t,c) = (take c x) ++ [t] ++ (Com.sub x (c + 1))
