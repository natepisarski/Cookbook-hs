--Cookbook.Ingredients.Tupples.Look
--This library is for manipulating and searching lists of two-element tupples.

module Cookbook.Ingredients.Tupples.Look(look,lookList,swp) where

-- | Returns the second element of the first tupple where the first element matches input.
look :: (Eq a) => [(a,b)] -> a -> (Maybe b)
look [] _ = Nothing
look (a:b) c = if fst a == c then Just (snd a) else look b c

-- | Returns all second elements where (fst t) matches the input.
lookList :: (Eq a) => [(a,b)] -> a -> [b]
lookList a b =  [d | (c,d) <- a, c == b]

-- | Swap the order of a second-degree tupple.
swp :: (a,b) -> (b,a)
swp    (a,b)  = (b,a)
