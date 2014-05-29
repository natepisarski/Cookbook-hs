{- |
   Module      :   Cookbook.Project.Quill2.Q2Parse
   Copyright   :   (c) 2014 by Nate Pisarski
   License     :   BSD3
   Maintainer  :   nathanpisarski@gmail.com
   Stability   :   Stable
   Portability :   Portable (Cookbook)
Q2Parse is the "engine" of Quill2. It is able to turn a string (or a list of them) into a Quill database. The syntax of Quill currently supports Whitespace-inclusive strings, comments, and whitespace-independence.
-}

module Cookbook.Project.Quill2.Q2Parse where

import qualified Cookbook.Essential.Common         as Cm
import qualified Cookbook.Essential.Continuous     as Ct

import qualified Cookbook.Ingredients.Lists.Modify    as Md
import qualified Cookbook.Ingredients.Lists.Access    as Ac
import qualified Cookbook.Ingredients.Lists.Encompass as En

import Cookbook.Project.Quill2.Q2Prelude

-- | Remove all C-style comments frm the code. Does not support single-line comments, because Quill2 is fully whitespace independant.
decomment :: String -> String
decomment = (`Ct.splice` ("/*", "*/"))

-- | Prepare the lines of a file for processing.
prepare :: [String] -> String
prepare = decomment . (`Ct.remove` '\n') . unlines

-- | Process a single entry in the database into a Table.
pTable :: String -> Quill
pTable x = typ
  where
    name = Ct.between (Ct.before x '{') ('(',')') --table(name) or list(name) gets name.
    typ  = (name,if Ct.before x '{' `Ac.contains` "table" then Table (parseTables body) else List body) -- Determines what kind of type the Quill is.
    body = Md.splitOn (En.encompassing x ('{','}')) ';' 
    quoted y = if ('`','\'') `Ac.surrounds` y then En.encompassing x ('`','\'') else Ct.remove y ' ' -- Whole String encapsulation
    parseTables = map (\x -> (quoted $ Ct.before x ':', Ct.after x ':'))

-- | Turn the lines of a file into a list of tables, AKA a Database.
pFile :: [String] -> [Quill]
pFile x = map pTable $ Md.splitOn (prepare x) '}'



