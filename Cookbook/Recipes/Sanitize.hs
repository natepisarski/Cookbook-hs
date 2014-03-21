{- |
   Module      :   Cookbook.Recipes.Sanitize
   Copyright   :   (c) 2014 by Nate Pisarski
   License     :   BSD3
   Maintainer  :   nathanpisarski@gmail.com
   Stability   :   Stable
   Portability :   Portable (Cookbook)
Library for sanitizing data, mainly strings.
-}

module Cookbook.Recipes.Sanitize where

import qualified Cookbook.Essential.Common             as Cm
import qualified Cookbook.Ingredients.Lists.Modify     as Md
import qualified Cookbook.Ingredients.Lists.Access     as Ac
import qualified Cookbook.Ingredients.Functional.Break as Br

-- | Restricts an entire list of information from appearing in another list bit-by-bit.
blacklist :: (Eq a) => [a] -> [a] -> [a]
blacklist x c = Cm.apply (map (flip Md.rm) c) x

-- | Removes a leading character from a list.
rmleading :: (Eq a) => [a] -> a -> [a]
rmleading x c = Br.filterBreak (==c) x

-- | Refpos wrapper for two lists, last to first.
up :: (Eq a) => ([a],[a]) -> [a] -> [a]
up (a,b) = map (Ac.refpos (a,b)) 

-- | Refpos wrapper for two lists, first to last.
down :: (Eq a) => ([a],[a]) -> [a] -> [a]
down (a,b)  = map (Ac.refpos (b,a)) 

-- | Removes all doubles in the list, turning them into just one occurrence.
rmdb :: (Eq a) => [a] -> [a]
rmdb [] = []
rmdb [x] = [x]
rmdb (x:y:zs) = x: rmdb (if x == y then Br.removeBreak (== x) (y:zs) else y:zs)

-- | Wholly removes doubles from a list.
rmdbAll :: (Eq a) => [a] -> [a]
rmdbAll [] = []
rmdbAll (x:xs) = x : rmdb (Md.rm xs x)

-- | Moves a string to lower-case.
tolower :: String -> String
tolower = down (['a'..'z'],['A'..'Z'])

-- | Moves a string to upper-case.
toupper :: String -> String
toupper = up (['a'..'z'],['A'..'Z'])

-- | Removes all of the leading whitespace from a string.
rmlws :: String -> String
rmlws = (`rmleading` ' ')

-- | Removes all "symbols" from a string
rmsymbols :: String -> String
rmsymbols = (`blacklist` ['\\'..'`'])
