{- |
   Module      :   Cookbook.Project.Scribe.Scribe
   Copyright   :   (c) 2014 by Nate Pisarski
   License     :   BSD3
   Maintainer  :   nathanpisarski@gmail.com
   Stability   :   Stable
   Portability :   Portable (Cookbook)
Scribe is an advanced database system with infinitely recursive data structures. It supports tables, inner tables, inner-inner tables, etc. There is no distinction between "rows" or "columns". Instead, the table itself is a recursive data structure capable of encapsulating itself. For added benefit, Scribe entries can contain promises, which are references to tables already defined in the file.
-}

module Cookbook.Project.Scribe.Scribe where

import qualified Cookbook.Essential.Common         as Cm
import qualified Cookbook.Essential.Continuous     as Ct
import qualified Cookbook.Ingredients.Lists.Modify as Md
import qualified Cookbook.Recipes.Sanitize         as Sn

-- Standard characters
tblStrt  = '{'
tblEnd   = '}'
rlvalSep = '='
itemSep  = ';'
cmtStart = "(*"
cmtEnd   = "*)"
glblTblS = '.'

--Helper functions
isTable x = and [tblStrt `elem` x,tblEnd `elem` x]

--Data types

-- | Tree-like data structure for containing Tables. Promises are not an underlying feature of the data type, but are an integral feature of Scribe. Thus, they must be implemented in the parsing phase.
data Table = Entry (String,String) | Table (String,Table)

--Parsing functions

-- | Prepares a list of strings for parsing. If you only have a String, wrap it in []'s. Removes comments at this point.
prepare :: [String] -> String
prepare = flip Ct.splice (cmtStart,cmtEnd) . flip Sn.blacklist ['\t','\n'] . unlines

-- | Parses one global table into a Table. Should not be called directly, outside of adding and removing tables from a database.
parseTable :: String -> Table --table1{a:b;c:{a:b;c:d;};}
parseTable x
  | isTable (Cm.fromLast (
  where inner = encompassingScope x (tblStrt,tblEnd)
