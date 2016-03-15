{- |
   Module      :   Cookbook.Ingredients.Lists.Stats
   Copyright   :   (c) 2014 by Nate Pisarski
   License     :   BSD3
   Maintainer  :   nathanpisarski@gmail.com
   Stability   :   Stable
   Portability :   Portable (Cookbook)
Library for determining mathematically whether two lists are similar.
-}

module Cookbook.Ingredients.Lists.Stats where

import qualified Cookbook.Ingredients.Lists.Access     as Ac
import qualified Cookbook.Ingredients.Lists.Modify     as Md
import qualified Cookbook.Ingredients.Tupples.Assemble as As

-- | Creates a list with the frequency of elements in a list.
frequency :: (Eq a) => [a] -> [(a,Int)] 
frequency x = let y = map (\c -> (c,Ac.count x c)) x in As.rmDb y

-- | Returns the x-amount of most frequent elements in a list. If there is a "tie", the order it appears in a list takes precedence. 
mostFrequent :: (Eq a) => [a] -> Int -> [a]
mostFrequent x c = take c $ Md.rev (As.assemble  $ frequency x)
