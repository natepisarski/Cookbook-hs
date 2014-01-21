{- |
   Module      :   Cookbook.Ingredients.Tupples.Look
   Copyright   :   (c) 2014 by Nate Pisarski
   License     :   BSD3
   Maintainer  :   nathanpisarski@gmail.com
   Stability   :   Stable
   Portability :   Portable (Standalone - ghc)

Library for using tupples as an associative list. It can be thought of like a "map" in Clojure, or a database of key-value pairs. This is also the general-purpose tupple library, so there's stragglers.
-}

module Cookbook.Ingredients.Tupples.Look(look,lookList,swp,rmLook,group) where

-- | Look up an lval in a list of tupples.
look :: (Eq a) => [(a,b)] -> a -> (Maybe b)
look [] _ = Nothing
look (a:b) c = if fst a == c then Just (snd a) else look b c

-- | Returns all elements where the lval matches the element.
lookList :: (Eq a) => [(a,b)] -> a -> [b]
lookList a b = map snd $ filter (\(c,d) -> c == b) a

-- | Swap the lval with the rval, and vice-versa.
swp :: (a,b) -> (b,a)
swp    (a,b)  = (b,a)

-- | Removes all matches of the lval.
rmLook :: (Eq a) => [(a,b)] -> a -> [(a,b)]
rmLook [] _ = []
rmLook ((a,b):cs) d = if a == d then cs else (a,b) : rmLook cs d

-- | Turns a list into as many double-tupples (a,a) as it can, dropping items from uneven lists.
group :: [a] -> [(a,a)]
group [] = []
group [x] = []
group (x:y:z) = (x,y) : group z
