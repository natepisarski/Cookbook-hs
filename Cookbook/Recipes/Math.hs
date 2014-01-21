{- |
   Module      :   Cookbook.Recipes.Math
   Copyright   :   (c) 2014 by Nate Pisarski
   License     :   BSD3
   Maintainer  :   nathanpisarski@gmail.com
   Stability   :   Stable
   Portability :   Portable (Standalone - ghc)
Library for working with numbers more easily. It includes somewhat lazy functions for when a lambda will clutter code up too much, as well as more involved mathematical formulae. 
-}

module Cookbook.Recipes.Math where

-- | Increase value by one.
inc :: (Enum a) => a -> a
inc = succ

-- | Decrease value by one,
dec :: (Enum a) => a -> a
dec = pred

-- | Multiply a number by itself.
sqr :: (Num a) => a -> a
sqr x = (x*x)

-- | Find the average of a group of Fractionals.
avg :: (Fractional a) => [a] -> a
avg x = sum x / (realToFrac $ length x)

-- | Find the standard deviation of a list of data.
stdev :: (Fractional a, Floating a) => [a] -> a
stdev x = sqrt $ diffs / realToFrac (length x)
  where diffs = sum $ [sqr $ a - avg x| a <- x]

-- | Factorial, from 1 to point.
fact :: (Num a, Enum a) => a -> a
fact x = foldl (*) x [1..dec x]
