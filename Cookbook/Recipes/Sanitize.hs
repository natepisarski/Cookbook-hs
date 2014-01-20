module Cookbook.Recipes.Sanitize where
--"Sanization" is quite the overloaded term. The purpose of this sanitzation lib
--is to be as unobtrusive yet flexible as possible. This will generally work with
--strings, so some domain-specific solutions for strings are present in this lib

import qualified Cookbook.Essential.Common             as Cm
import qualified Cookbook.Ingredients.Lists.Modify     as Md
import qualified Cookbook.Ingredients.Lists.Access     as Ac
import qualified Cookbook.Ingredients.Functional.Break as Br

blacklist :: (Eq a) => [a] -> [a] -> [a]
blacklist x c = Cm.apply (map (flip Md.rm) c) x

rmleading :: (Eq a) => [a] -> a -> [a]
rmleading x c = Br.filterBreak (==c) x

up :: (Eq a) => ([a],[a]) -> [a] -> [a]
up (a,b) c = map (Ac.refpos (a,b)) c

down :: (Eq a) => ([a],[a]) -> [a] -> [a]
down (a,b) c = map (Ac.refpos (b,a)) c

rmdb :: (Eq a) => [a] -> [a]
rmdb [] = []
rmdb [x] = [x]
rmdb (x:y:zs) = if x == y then x: rmdb (Br.removeBreak (== x) (y:zs)) else x : rmdb (y:zs)

rmdbAll :: (Eq a) => [a] -> [a]
rmdbAll [] = []
rmdbAll (x:xs) = x : rmdb (Md.rm xs x)

tolower :: String -> String
tolower = down (['a'..'z'],['A'..'Z'])

toupper :: String -> String
toupper = up (['a'..'z'],['A'..'Z'])

rmlws :: String -> String
rmlws = ((flip rmleading) ' ')

rmsymbols :: String -> String
rmsymbols = ((flip blacklist) (['\\'..'`']))
