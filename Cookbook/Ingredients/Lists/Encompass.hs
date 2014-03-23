module Cookbook.Ingredients.Lists.Encompass where
{- |
   Module      :   Cookbook.Ingredients.Lists.Encompass
   Copyright   :   (c) 2014 by Nate Pisarski
   License     :   BSD3
   Maintainer  :   nathanpisarski@gmail.com
   Stability   :   Stable
   Portability :   Portable (Cookbook)
Encompass is a library for parsing that is stringent on scope. It supports scope monitoring on single sets of single elements in a list, but it does not need to be a string. 
-}
import qualified Cookbook.Essential.Continuous as Ct
import qualified Cookbook.Essential.Common     as Cm
import qualified Cookbook.Ingredients.Functional.Break as Br
-- | Get the entire section of a list contained within the scope delimited by the parameters, even sub-scopes.
encompassing :: (Eq a) => [a] -> (a,a) -> [a]
encompassing (x:xs) (a,b) = helper (x:xs) (a,b) 0
  where
    helper [] _ _ = []
    helper (y:ys) (c,d) e
      | y == d && e <= 1 = []
      | y == c = condInp    $ helper ys (c,d) (e + 1)
      | y == d = y : helper ys (c,d) (e - 1)
      | otherwise = condInp $ helper ys (c,d) e
      where
        condInp = ([y | e > 0] ++)

-- | Partial implementation of after, but working with scopes. Dangerous implementation, use with caution.
afterEncompassing :: (Eq a) => [a] -> (a,a) -> [a]
afterEncompassing a (b,c) = tail $ Ct.after a $ encompassing a (b,c)

-- | Partial implementation of before, but working with scopes. Less dangerous implementation than afterEncompassing, but still dangerous.
beforeEncompassing :: (Eq a) => [a] -> (a,a) -> [a]
beforeEncompassing a (b,c) = let temp = Ct.before a $ encompassing a (b,c) in take (length temp - 1) temp

-- | Gets all of the elements outside a given scope.
splitEncompassing :: (Eq a) => [a] -> (a,a) -> [[a]]
splitEncompassing a (b,c) = filter (\x -> x /= []) (helper a (b,c)) 
  where
    helper e (f,g)
      | not $  f `elem` e && g `elem` e = [e]
      | otherwise = beforeEncompassing e (f,g) : splitEncompassing (afterEncompassing e (f,g)) (f,g)

-- | There is a long story behind this function, and why it sucks. See ENN1 in the source.
notEncompassedSplit :: (Eq a) => [a] -> (a,a) -> a -> [[a]]
notEncompassedSplit [] _ _ = [] -- "b{c,d},e,{f:g,e:h}"
notEncompassedSplit ls (sp1,sp2) sp = toGaps $ helper ls 0
  where
    helper [] _ = []
    helper (x:xs) c
      | x == sp1 && c >= 0 = (sp1: encompassing (x:xs) (sp1,sp2)) : helper (afterEncompassing xs (sp1,sp2)) (c + 1)
      | x == sp2 && c <= 0 = [x] :  helper xs (c - 1)
      | x == sp1 = [x] : helper xs (c+1)
      | x == sp2 = [x] : helper xs (c - 1)
      | x == sp && c > 0 = [x] : helper xs c
      | x == sp && c <= 0 = [] : helper xs c
      | otherwise = [x] : helper xs c

toGaps :: (Eq a) => [[a]] -> [[a]]
toGaps [] = []
toGaps x
  | [] `notElem` x = [Cm.flt x]
  | otherwise = (Cm.flt (Br.filterBreak (\c -> c /= []) x)) : (toGaps $ tail (Br.removeBreak (\c -> c /= []) x))

-- | Returns all of the elements inside of a scope.
gatherEncompassing :: (Eq a) => [a] -> (a,a) -> [[a]]
gatherEncompassing a (b,c) 
  | not $ b `elem` a && c `elem` a = []
  | otherwise = encompassing a (b,c) : gatherEncompassing (afterEncompassing a (b,c)) (b,c)

-- [ENN1]
-- This function is terrible. Bloody AWFUL. So, why include it? Because it works.
-- the implementation of the function is hard to rework, so unfortunately it doesn't
-- get much better than this. I've tried rereading it time and time again, and the
-- details of how it works confuse me. Prior to making the function, I spent
-- more than a month thinking about how it should work. After that didn't work
-- I began to change variables and behaviors in a fit of rage, and it started
-- to work. And don't question the helper function either. This is as good
-- as it gets, ladies and gentlemen.
