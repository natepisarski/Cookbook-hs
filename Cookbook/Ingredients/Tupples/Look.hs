module Cookbook.Ingredients.Tupples.Look(look,lookList,swp) where

-- | Returns the second element of the first tupple where the first element matches input.
look :: (Eq a) => [(a,b)] -> a -> (Maybe b)
look [] _ = Nothing
look ((a,b):bs) c
  | a == c = (Just b)
  | otherwise = look bs c

-- | Returns all second elements where (fst t) matches the input.
lookList :: (Eq a) => [(a,b)] -> a -> Maybe [b]
lookList d c = case filt of [] -> Nothing ; _ -> Just filt
  where filt = [b | (a,b) <- d, a == c]

-- | Swap the order of a second-degree tupple.
swp :: (a,b) -> (b,a)
swp (a,b) = (b,a)
