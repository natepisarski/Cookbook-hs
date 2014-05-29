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
import qualified Cookbook.Recipes.Sanitize             as Sn
import qualified Cookbook.Recipes.Math                 as Ma

-- | Creates a list with the frequency of elements in a list.
frequency :: (Eq a) => [a] -> [(a,Int)] 
frequency x = let y = map (\c -> (c,Ac.count x c)) x in As.rmDb y

-- | Returns the x-amount of most frequent elements in a list. If there is a "tie", the order it appears in a list takes precedence. 
mostFrequent :: (Eq a) => [a] -> Int -> [a]
mostFrequent x c = take c $ Md.rev (As.assemble  $ frequency x)

-- | Provides a mathematical score out of 1 based on the similarities between the two words. This is freqScore, but it takes into account length.
wordscore :: (Eq a) => [a] -> [a] -> Double
wordscore a b = (freqScore a b - 0.1) + (0.1 / realToFrac (if diffLen == 0 then 1 else diffLen))
  where diffLen = abs $ length a - length b

-- | Provides a frequency score between two lists.
freqScore :: (Eq a) => [a] -> [a] -> Double
freqScore a b =  rawFreq / fromIntegral diffLen
  where diffLen = fromIntegral  $ length (frequency (if length (frequency a) < length (frequency b) then a else b))
        rawFreq = fromIntegral (sum $ map (\e -> if e `elem` d then 1 else 0) c)
        (c:d:_) = map (`mostFrequent` diffLen) [a,b]
