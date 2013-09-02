module Cookbook.Common(sub,positions,pos) where

--Cut a list off at a certain point
--sub [1,2,3] 1 -> [2 3]
sub :: (Eq a) => [a] -> Int -> [a]
sub [] _ = []
sub x 0 = x
sub (x:xs) c = sub xs (c - 1)

--Find all positions of an element in a list
--positions [1,2,1,2,1,2] 1 -> [ 0 2 4 ]
--Edge cases: [] on notElem
positions :: (Eq a) => [a] -> a -> [Int]
positions x c = let y = zip x [0..(length x)] in find y
  where find y = [e | (d,e) <- y, d == c]

--Interface to positions to find the first matching element.
--pos [1,2,3] 2 -> 1
--Edge Cases: -1 on notElem (Maybe unused because this is a helper function)
pos :: (Eq a) => [a] -> a -> Int
pos x c | c `notElem` x = -1
pos x c = let ans = positions x c in (if (length ans) > 1 then (head . tail) else head) ans
          


