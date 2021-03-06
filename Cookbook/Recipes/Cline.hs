module Cookbook.Recipes.Cline(parse,Cline(..),clineExtract,clArg) where

import qualified  Cookbook.Essential.Continuous     as Ct
import qualified  Cookbook.Ingredients.Lists.Modify as Md
import qualified  Cookbook.Ingredients.Lists.Access as Ac
import qualified  Cookbook.Recipes.Sanitize         as Sn

{- |
   Module      :   Cookbook.Recipes.Cline
   Copyright   :   (c) 2014 by Nate Pisarski
   License     :   BSD3
   Maintainer  :   nathanpisarski@gmail.com
   Stability   :   Stable
   Portability :   Portable (Cookbook)
Cline is a library for managing strings meant to represent command-line arguments. Flags can toggle functionality in programs. Command-line arguments and their short names take an argument.
-}

-- | Represents command-line options. Works on either arguments or flags.
data Cline = Flag Char | Argument String String deriving (Show,Eq)

-- | Parses a flag argument.
singleParse :: String -> Cline
singleParse y = Flag (last y)

-- | Parses an argument Cline.
doubleParse :: (String, String) -> Cline
doubleParse (a,b) = Argument (Ct.after a "--") b

-- | Extract arguments from a list of word-separated strings.
clineExtract :: [String] -> [Cline]
clineExtract x = case x of
  (a:b:[]) -> if Ac.count a '-' == 2 then [doubleParse (a,b)] else [singleParse a,singleParse b]
  (a:[]) -> [singleParse a]
  _ -> helper x

-- | Helps clineExtract. Determines whether to single or double parse something
helper :: [String] -> [Cline]
helper x
  | Ac.count w1 '-' == 1 = singleParse w1 : clineExtract (w2:r)
  | Ac.count w1 '-' == 2 = doubleParse (w1,w2) : clineExtract r
  | otherwise = clineExtract (w2:r)
    where (w1:w2:r) = x

-- | Parse a flat string into a list of Clines.
parse :: String -> [Cline]
parse =  clineExtract . (`Md.splitOn` ' ')

-- Helpers

-- | Cleans a command line argument. If it's not a number, it will add a couple of dashes in front of it.
clArg :: String -> String
clArg x = if '-' `notElem` x && Sn.notNumeral x then ("--" ++ x) else x
