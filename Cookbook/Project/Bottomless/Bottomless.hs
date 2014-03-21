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
import qualified Cookbook.Essential.Continuous         as Ct
import qualified Cookbook.Essential.Common             as Cm

-- | Elementary data type of tables. Tables map strings (identifiers) with a Btmliteral, which can be an array (list of Btmliterals, usually Items), Items, or more Tables.
data Btmliteral a = Table (String, [(String, Btmliteral a)]) | Array [Btmliteral a] | Item a deriving (Show,Eq)

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

-- Here's where you're getting stuck:
-- You NEED to split up the global tables from the rest of the tables in the parse. This behavior recurses down onto all tables. Make
-- a function that splits outside of a scope, and fix encompassingScope





{-
-- String will look like: tabName{a:b,b:[a,b,c,d],d:{a:b}}
-- | Parse a table within the file.
parseTable :: String -> Btmliteral
parseTable x = Table ((Ct.before x '{'),parse Ct.after x '{')

-- | General parsing function
parse :: String -> [Btmliteral]
parse x
  | head (afterthe ':') = '{' = paraseTable $ afterthe ':' : parse $ nextScope x -- Figure out nextScope
  | head (afterthe ':') = '[' = parseArray $ encompassingScope x '[' '
  | otherwise =
    where afterthe = Ct.after x
-}
