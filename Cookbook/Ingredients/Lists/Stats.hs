module Cookbook.Ingredients.Lists.Stats where

import qualified Cookbook.Ingredients.Lists.Access     as Ac
import qualified Cookbook.Ingredients.Lists.Modify     as Md
import qualified Cookbook.Ingredients.Tupples.Assemble as As
import qualified Cookbook.Recipes.Sanitize             as Sn
import qualified Cookbook.Recipes.Math                 as Ma

frequency :: (Eq a) => [a] -> [(a,Int)] 
frequency x = let y = map (\c -> (c,Ac.count x c)) x in As.rmDb y

mostFrequent :: (Eq a) => [a] -> Int -> [a]
mostFrequent x c = take c $ Md.rev (As.assemble  $ frequency x)

wordscore :: (Eq a) => [a] -> [a] -> Double
wordscore a b = (freqScore a b - 0.1) + (0.1 / realToFrac (if diffLen == 0 then 1 else diffLen))
  where diffLen = (abs $(length a) - (length b))

freqScore :: (Eq a) => [a] -> [a] -> Double
freqScore a b =  (rawFreq / (fromIntegral diffLen))
  where diffLen = fromIntegral (if (length (frequency a)) < (length (frequency b)) then length (frequency a) else length (frequency b))
        rawFreq = fromIntegral ((sum $ (map (\e -> if e `elem` d then 1 else 0) c)))
        (c:d:_) = map ((flip mostFrequent) diffLen) [a,b] 

