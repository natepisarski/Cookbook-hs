{- |
   Module      :   Cookbook.Ingredients.Lists.Modify
   Copyright   :   (c) 2014 by Nate Pisarski
   License     :   BSD3
   Maintainer  :   nathanpisarski@gmail.com
   Stability   :   Stable
   Portability :   Portable (Cookbook)
Library for modifying data within a list, and transforming lists in certain ways. While that's vague, so is the definition of "To Modify". 
-}
module Cookbook.Ingredients.Lists.Modify where

import qualified Cookbook.Essential.Common             as Cm
import qualified Cookbook.Essential.Continuous         as Ct
import qualified Cookbook.Ingredients.Functional.Break as Br
import qualified Cookbook.Ingredients.Lists.Access     as Ac

-- | Reverses a list.
rev :: [a] -> [a]
rev [] = []
rev (x:xs) = rev xs ++ [x]

-- | Removes all occurances of an element from a list. See MDN1 in the source for a run-down on why it's implemented here and in Continuous.
rm :: (Eq a) => [a] -> a -> [a] -- See MDN1
rm x c = filter (/=c) x

-- | Splits a list on an element, making a list of lists based on the element as a seperator.
splitOn :: (Eq a) => [a] -> a -> [[a]]
splitOn [] _ = []
splitOn x c = if c `notElem` x then [x] else Ct.before x c : splitOn (Ct.after x c) c

-- | Returns the data between two items.
between :: (Eq a) => [a] -> (a,a) -> [a]
between a (c,d) = Ct.after (take (last $ Cm.positions a d) a) c

-- | Implementation of between that works on a list of lists, and using Contains rather than elem.
linesBetween :: (Eq a) => [[a]] -> ([a],[a]) -> [[a]]
linesBetween a (c,d) = tail $ Br.filterBreak (\e -> not $ Ac.contains e d) $ Br.removeBreak (\e -> not $ Ac.contains e c) a

-- | Put an element after every element of a list, not including the last element.
intersperse :: [a] -> a -> [a]
intersperse [x] _ = [x]
intersperse (x:xs) c =  x:c : intersperse xs c

-- | An equivelant to the terenary operator for Haskell.
(?) :: a -> a -> Bool -> a
(?) a b c = if c then a else b

-- | Returns a list of all elements surrounded by elements. Basically a between which works more than once.
surroundedBy :: (Eq a) => [a] -> (a,a) -> [[a]] -- Map checks if a or b are not elememts.
surroundedBy x (a,b) = if any (not . flip elem x) [a, b] then [] else recurse
  where recurse = Ct.before (Ct.after x a) b : surroundedBy (Ct.after x b) (a,b)
                    
-- [MDN1]
-- This is implemented in Continuous, but is kept here for historical purposes,
--    or for when Continuous is too heavy-duty for whatever job you are on.

-- [MDN2]
-- encompassingScope has been renamed to "encompassing" and moved to its own module with similar functions, Cookbook.Ingredients.Lists.Encompass
