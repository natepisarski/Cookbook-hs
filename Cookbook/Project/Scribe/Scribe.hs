
--Cookbook.Project.Scribe.Scribe
--Library for reading scribe databases
module Cookbook.Project.Scribe.Scribe(Table,Database,isTable,makeTable,parse,table) where

import qualified Cookbook.Essential.Continuous             as Ct
import qualified Cookbook.Essential.Common                 as Cm
import qualified Cookbook.Ingredients.Tupples.Look      as Lk
import qualified Cookbook.Ingredients.Lists.Access        as Ac
import qualified Cookbook.Ingredients.Lists.Modify         as Md
import qualified Cookbook.Ingredients.Lists.Replace       as Rp
import qualified Cookbook.Ingredients.Functional.Break as Br
import qualified Cookbook.Recipes.Text                             as Tx

import Data.Maybe
--Helper functions
isTable :: String -> Bool
isTable x = and [x `Ac.contains` "table",'{' `elem` x]

isEntry :: String -> Bool
isEntry x = and [x `Ac.contains` "item",':' `elem` x]

tupplize :: [a] -> [(a,a)]
tupplize [] = []
tupplize [a] = []
tupplize (a:b:cs) = (a,b) : tupplize cs

tableData :: [String] -> [(String,String)]
tableData [] = [] -- It works. Don't touch it. If you do, you will regret it. Is it a good idea to have it in one line? No. But it's too fragile to change
tableData (x:xs) =  if isEntry x then (tupplize $ let (unclean:c:_) = Md.splitOn x ':' in [Rp.replace unclean ("item ",""),c]) ++ tableData xs else tableData xs

makeTable :: [String] -> Table
makeTable (x:xs) = (Md.rm (Ct.after x "table ") '{',entries)
  where entries = tableData $ Tx.linesBetween (x:xs) ("{","}")
--

--Main logic
type Table = (String,[(String,String)])
type Database = [Table]

parse :: [String] -> Database
parse (x:xs) = if isTable x then makeTable (x:xs) : (parse (afterTable xs)) else parse xs
  where afterTable c = (Br.removeBreak (\d -> '}' `notElem` d) c)

table :: String -> Database -> [(String,String)]
table c x = let i = Lk.look x c in case i of (Just i) -> i; Nothing -> []
--


