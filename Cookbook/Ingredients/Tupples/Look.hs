--Cookbook.Ingredients.Tupples.Look
--This library is for manipulating and searching lists of two-element tupples.

module Cookbook.Ingredients.Tupples.Look(look,lookList,swp,rmLook,group) where

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

-- | Reverse this.look. Will remove any matches.
rmLook :: (Eq a) => [(a,b)] -> a -> [(a,b)]
rmLook [] _ = []
rmLook ((a,b):cs) d = if a == d then cs else (a,b) : rmLook cs d

-- | Turn a list into as many bifold tupples as possible.
group :: [a] -> [(a,a)]
group [] = []
group [x] = []
group (x:y:z) = (x,y) : group z
