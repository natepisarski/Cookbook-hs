--Cookbook.Essential.Common
--Common is the root of the Cookbook library. It provides the abstracts commonly used to build
--robust applications and sub-libraries within Cookbook. In any Cookbook implementation, Common
--varies based on the language features the language presents. In Haskell, Common merely provides
--informational contexts for modifying/accessing lists. They are placed in this high of a module because
--of how ubiquitous their use is, even though they fit in with Cookbook.Ingredients.Lists

module Cookbook.Essential.Common(sub,positions,pos,apply,flt,fromLast) where

-- | Returns a new list starting at a position. List positions start at 0.
sub :: (Eq a) => [a] -> Int -> [a]
sub [] _ = []
sub x 0 = x
sub (x:xs) c = sub xs (c - 1)

-- | Finds every occurence of an element within a list, starting at position 0.
positions :: (Eq a) => [a] -> a -> [Int]
positions x c = let y = zip x [0..(length x)] in find y
  where find y = [e | (d,e) <- y, d == c]

-- | Interface to position for finding the position of the first occurence in a list.
pos :: (Eq a) => [a] -> a -> Int
pos x c | c `notElem` x = -1
pos x c = let ans = positions x c in ((if (length ans) > 1 then (head . tail) else head) ans)

-- | Apply a list of functiosn to one value, using the result from the function beforehand.
apply :: [(a -> a)] -> a -> a
apply [] c = c
apply (f:fs) c = apply fs (f c)

-- | Flatten a list one level.
flt :: [[a]] -> [a]
flt [] = []
flt (x:xs) = x ++ flt xs

fromLast :: ([a] -> [a]) -> [a] -> [a]
fromLast f c = rev $ f $ rev c
  where rev [] = []
        rev (x:xs) = rev xs ++ [x]
