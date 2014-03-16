module Cookbook.Recipes.Cline(parse) where

import qualified  Cookbook.Essential.Continuous     as Ct
import qualified  Cookbook.Essential.Common         as Cm
import qualified  Cookbook.Ingredients.Lists.Modify as Md
import qualified  Cookbook.Ingredients.Lists.Access as Ac

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

--                                  ARGS1   ARGS2   F1
-- Cline looks like program_name --command argument -f

-- Extract the command line arguments into parsable tokens.
-- Will look like [--thing,arg,-f]
singleParse y = Flag (last y)
doubleParse (a,b) = Argument (Ct.after a "--") b

clineExtract :: [String] -> [Cline]
clineExtract x = case x of
  (a:b:[]) -> if Ac.count a '-' == 2 then [doubleParse (a,b)] else ([(singleParse a),(singleParse b)])
  (a:[]) -> [singleParse a]
  _ -> helper x
  
helper x
  | Ac.count w1 '-' == 1 = (singleParse w1) : (clineExtract (w2:r))
  | Ac.count w1 '-' == 2 = doubleParse (w1,w2) : (clineExtract r)
  | otherwise = clineExtract (w2:r)
  where (w1:w2:r) = x

parse :: String -> [Cline]
parse =  clineExtract . ((flip Md.splitOn) ' ')
