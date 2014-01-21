{- |
   Module      :   Cookbook.Ingredients.Tupples.Assemble
   Copyright   :   (c) 2014 by Nate Pisarski
   License     :   BSD3
   Maintainer  :   nathanpisarski@gmail.com
   Stability   :   Stable
   Portability :   Portable (Standalone - ghc)

Library for arranging and modifying lists of twopples. Dubber Tuppers? There has to be a better word for this. It works on two-sided Tupples.
-}

module Cookbook.Ingredients.Tupples.Assemble where

-- | Sorts a list of Tupples based on their second element.
tupsort :: (Ord b) => [(a,b)] -> [(a,b)]
tupsort [] = []
tupsort ((a,n):xs) = lesser ++ [(a,n)] ++ greater
  where lesser   = tupsort [(c,d) | (c,d) <- xs, d <= n]
        greater = tupsort [(c,d) | (c,d) <- xs, d > n]

-- | Order a list of tupples by their rval, and collect the lvals as a list.
assemble :: (Ord b) => [(a,b)] -> [a]
assemble = (map fst) . tupsort

-- | Removes all double-entries from a list of tupples, contingent on just lval.
rmDb :: (Eq a) =>[(a,b)] -> [(a,b)]
rmDb [] = []
rmDb ((a,b):c) = (a,b) : rmDb [(d,e) | (d,e) <- c, d /= a]
