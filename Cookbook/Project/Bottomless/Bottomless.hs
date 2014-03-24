module Cookbook.Project.Bottomless.Bottomless where
{- |
   Module      :   Cookbook.Project.Bottomless.Bottomless
   Copyright   :   (c) 2014 by Nate Pisarski
   License     :   BSD3
   Maintainer  :   nathanpisarski@gmail.com
   Stability   :   Stable
   Portability :   Portable (Cookbook)
Bottomless is an infinitely recursive database flat file language with table and list literals.
-}

-- | Elementary data type of tables. Tables map strings (identifiers) with a Btmliteral, which can be an array (list of Btmliterals, usually Items), Items, or more Tables.

import qualified Cookbook.Ingredients.Lists.Access     as Ac
import qualified Cookbook.Ingredients.Functional.Break as Br
import qualified Cookbook.Ingredients.Lists.Modify     as Md
import qualified Cookbook.Ingredients.Lists.Encompass  as En
import qualified Cookbook.Essential.Continuous         as Ct
import qualified Cookbook.Essential.Common             as Cm

-- | Elementary data type of tables. Tables map strings (identifiers) with a Btmliteral, which can be an array (list of Btmliterals, usually Items), Items, or more Tables.
data Btmliteral a = Table (String, [Btmliteral a]) Item (String,a) deriving (Show,Eq)

-- General parsing functions
-- | Strip C-style comments from the file.
decomment :: [String] -> [String]
decomment [] = []
decomment (x:xs)
  | x `Ac.contains` "//" = Ct.before x "//"   : decomment xs -- Fixme #1 Dangerous comment stripping
  | x `Ac.contains` "/*" = Ct.before x "/*" : (decomment $ Br.removeBreak (\c -> not $ c `Ac.contains` "*/") xs)
  | x `Ac.contains` "*/" = decomment $ (Ct.after x "*/") : xs
  | otherwise = x: decomment xs

-- | Preprocess lines of a file for parsing.
prepare :: [String] -> String
prepare = Cm.flt . decomment

-- Project-specific parsing functions
-- | Parse the body of a table into a list of Btmliterals.
ptb :: [String] -> [Btmliteral String]
ptb [] = []
ptb (x:xs)
  | head (afterthe ':') == '{' = Table ((beforethe ':'),ptb (En.notEncompassedSplit (Ct.after x ":{") ('{','}') ',')) : ptb xs
  | otherwise = Item ((beforethe ':'),(afterthe ':')) : ptb xs
  where
    beforethe = Ct.before x
    afterthe = Ct.after x

-- | Turn a prepared string into a list of btmliterals.
pat :: String -> [Btmliteral String]
pat x
  | '{' `elem` x = Table (Ct.before x '{',ptb (En.notEncompassedSplit (En.encompassing x ('{','}')) ('{','}') ',')) : pat (En.afterEncompassing x ('{','}'))
  | otherwise = []

-- | Turn the contents of a file into a list of Btmliterals.
pfile :: [String] -> [Btmliteral String]
pfile = pat . prepare

-- Base table functions (non-recursive)
itLook :: [Btmliteral String] -> String -> Maybe (Btmliteral String)
itLook [] _ = Nothing
itLook (x:xs) y = case x of
  (Table a b) -> if a == y then Just x else itLook xs y
  (Item a)    -> if a == y then Just x else itLook xs y
  _           -> itLook xs y

addItem :: [Btmliteral String] -> Btmliteral String -> [Btmliteral String]
removeItem :: [Btmliteral String] -> Btmliteral String -> [Btmliteral String]
changeItem :: [Btmliteral String] -> Btmliteral String -> [Btmliteral String]
changeItem h k = case itLook of
  (Just a) -> 
  Nothing
