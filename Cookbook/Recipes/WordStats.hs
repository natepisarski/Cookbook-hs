module Cookbook.Recipes.WordStats where

import qualified Cookbook.Recipes.Math            as Ma
import qualified Cookbook.Ingredients.Lists.Stats as St

wordscore :: (Eq a) => [a] -> [a] -> Double
wordscore a b = (freqScore a b - 0.1) + (0.1 / realToFrac (if diffLen == 0 then 1 else diffLen))
  where diffLen = (abs $(length a) - (length b))

freqScore :: (Eq a) => [a] -> [a] -> Double
freqScore a b =  (rawFreq / (fromIntegral diffLen))
  where diffLen = fromIntegral (if (length (St.frequency a)) < (length (St.frequency b)) then length (St.frequency a) else length (St.frequency b))
        rawFreq = fromIntegral ((sum $ (map (\e -> if e `elem` d then 1 else 0) c)))
        (c:d:_) = map ((flip St.mostFrequent) diffLen) [a,b] 
