module Cookbook.Ingredients.Functional.Break(
removeBreak, filterBreak,btr,imbreak) where

--Return the list at the first untrue return from predicate f.
--removeBreak (==1) [1,1,2,3] -> [2,3]
removeBreak :: (a -> Bool) -> [a] -> [a]
removeBreak _ [] = []
removeBreak f (c:cs)
  | not $ f c = (c:cs)
  | otherwise = removeBreak f cs

--Stop consing a list at first untrue return from predicate f.
--filterBreak (==1) [1,1,2,3] -> [1,1]
filterBreak :: (a -> Bool) -> [a] -> [a]
filterBreak _ [] = []
filterBreak f (c:cs)
  | not $ f c = []
  | otherwise = c : filterBreak f cs

-- Immediately break out of execution on a true.
-- Will return false at the end of execution
imbreak :: (a -> Bool) -> [a] -> Bool
imbreak _ [] = False
imbreak f x
  | f (head x) = True
  | otherwise = imbreak f (tail x)

--Boolean TRansform: (a,b) a if true, b if not, return the list.
--btr (==1) [1,2,3] ('a','b') -> "abb"
btr :: (a -> Bool) -> [a] -> (b,b) -> [b]
btr _ [] _ = []
btr f (c:cs) (a,b)
  | f c = a : rest
  | otherwise = b : rest
  where rest = btr f cs (a,b)
