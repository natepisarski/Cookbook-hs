--Cookbook.Recipes.Math
--A library for mathematical formulas, parsing, and conversions.
module Cookbook.Recipes.Math where

--Simple, helper arithmetic functions.
inc :: (Num a) => a -> a
inc = (+1)

dec :: (Num a) => a -> a
dec x = x - 1 -- Negatives throw off currying

sqr :: (Num a) => a -> a
sqr x = (x*x)

avg :: (Fractional a) => [a] -> a
avg x = sum x / (realToFrac $ length x)

--Basic formulae
stdev :: (Fractional a, Floating a) => [a] -> a
stdev x = sqrt $ diffs / realToFrac (length x)
  where diffs = sum $ [sqr $ a - avg x| a <- x]

-- Simple factorial, using fold.
fact :: Int -> Int
fact x = foldl (*) x [1..dec x]
