module Cookbook.Recipes.Detect(represent,toRepex,strpex,strmatch,containingPattern,withPattern) where

import Data.Maybe

import Cookbook.Common
import Cookbook.Ingredients.Lists.Access
import Cookbook.Ingredients.Functional.Break

-- | Represent a list using symbols, and if it's not found, return Nothing.
represent :: (Eq a) => [([a],b)] -> a -> (Maybe b)
represent [] _ = Nothing
represent ((a,b):c) item
  | item `elem` a = (Just b)
  | otherwise = represent c item

-- | Filter maybes out, replacing Nothings with a failsafe "catch-all"
toRepex :: (Eq a) => [([a],b)] -> [a] -> b-> [b]
toRepex a b failsafe= map (\c -> case c of (Just x) -> x;_ -> failsafe) (map (represent a) b)

--Standardized functions

-- | Standard interface to "toRepex" for strings.
strpex :: String -> String
strpex x = toRepex [(['a'..'z'],'@'),(['A'..'Z'],'!'),(['0'..'9'],'#'),([':'..'@']++['\\'..'`']++[' '..'/'],'&')] x '_'

-- | Does the string contain the standard strpex pattern?
strmatch :: String -> String -> Bool
strmatch x c = (strpex x) `contains` c

containingPattern :: [String] -> String -> [String]
containingPattern x c = filter (flip strmatch c) x

withPattern :: [String] -> String -> [String]
withPattern [] _ = []
withPattern a@(x:xs) c
  | strpex takeX == c = takeX : withPattern xs c
  | otherwise = withPattern xs c
  where takeX = (take (length c) x)

