module Cookbook.Recipes.Sanitize(blacklist,rmleading,up,down,rmdb,rmdbAll,tolower,toupper,rmlws,rmsymbols) where
--"Sanization" is quite the overloaded term. The purpose of this sanitzation lib
--is to be as unobtrusive yet flexible as possible. This will generally work with
--strings, so some domain-specific solutions for strings are present in this lib
import Cookbook.Common
import Cookbook.Ingredients.Lists.Modify
import Cookbook.Ingredients.Lists.Access
import Cookbook.Ingredients.Functional.Break

--Generic Underpinnings

-- | Remove each element of the second list from the first.
blacklist :: (Eq a) => [a] -> [a] -> [a]
blacklist x c = apply (map (flip rm) c) x

-- | Remove leading elements from the list. Useful for getting rid of spaces.
rmleading :: (Eq a) => [a] -> a -> [a]
rmleading x c = filterBreak (==c) x

-- | Move an element in one list to the other, if it is not there already.
up :: (Eq a) => ([a],[a]) -> [a] -> [a]
up (a,b) c = map (refpos (a,b)) c

-- | Up, but down. If that helps.
down :: (Eq a) => ([a],[a]) -> [a] -> [a]
down (a,b) c = map (refpos (b,a)) c

-- | Remove all adjacent doubles from the list.
rmdb :: (Eq a) => [a] -> [a]
rmdb [] = []
rmdb [x] = [x]
rmdb (x:y:zs) = if x == y then x: rmdb (removeBreak (== x) (y:zs)) else x : rmdb (y:zs)

-- | Removes absolutely all doubles, leaving the first copy.
rmdbAll :: (Eq a) => [a] -> [a]
rmdbAll [] = []
rmdbAll (x:xs) = x : rmdb (rm xs x)

--Interface to generic underpinnings

-- | Moves a string to lower case.
tolower :: String -> String
tolower = down (['a'..'z'],['A'..'Z'])

-- | Moves a string to upper case.
toupper :: String -> String
toupper = up (['a'..'z'],['A'..'Z'])

-- | Removes leading whitespace.
rmlws :: String -> String
rmlws = ((flip rmleading) ' ')

-- | Removes all symbols, leaving spaces intact.
rmsymbols :: String -> String
rmsymbols = ((flip blacklist) (['\\'..'`']))


