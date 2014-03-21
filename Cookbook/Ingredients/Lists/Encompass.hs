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

-- | Returns all of the elements inside of a scope.
gatherEncompassing :: (Eq a) => [a] -> (a,a) -> [[a]]
gatherEncompassing a (b,c)
  | not $ b `elem` a && c `elem` a = []
  | otherwise = encompassing a (b,c) : gatherEncompassing (afterEncompassing a (b,c)) (b,c)
