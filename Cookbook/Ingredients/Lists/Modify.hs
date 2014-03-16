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
import qualified Cookbook.Essential.Continuous         as Cnt
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
splitOn x c = if (c `notElem` x) then [x] else (Cnt.before x c) : splitOn (Cnt.after x c) c

-- | Returns the data between two items.
between :: (Eq a) => [a] -> (a,a) -> [a]
between a (c,d) = Cnt.after (take (last $ Cm.positions a d) a) c

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
surroundedBy x (a,b) = if or (map (not . (flip elem) x) [a,b]) then [] else recurse
  where recurse = Cnt.before (Cnt.after x a) b : surroundedBy (Cnt.after x b) (a,b)

-- | Returns the entire scope of an lval to an rval, including any internal scopes. For instance, the string \"here{is{a{string}}}\" is not easily parsable, even with Continuities. encompassingSc
encompassingScope :: (Eq a) => [a] -> (a,a) -> [a]
encompassingScope [] _ = []
encompassingScope a (c,d) = helper (Cnt.after a c) (c,d)
  where
    helper :: (Eq a) => [a] -> (a,a) -> [a]
    helper [] _ = []
    helper (x:xs) (e,f)
      | x == f = (if Ac.count xs f > 0 then x : helper xs (e,f) else []) -- See MDN2
      | otherwise = x : helper xs (e,f)
                    
-- [MDN1]
-- This is implemented in Continuous, but is kept here for historical purposes,
--    or for when Continuous is too heavy-duty for whatever job you are on.

-- [MDN2]
-- At the time of writing this note, this is the largest function in Cookbook.
-- How it works is, encompassingScope passes all elements after the first occurrence
-- of the rval to a helper function, helper, which will only keep collecting if it's
-- in scope. It determines this by checking to see if there are any more rvals left
-- in the list.
