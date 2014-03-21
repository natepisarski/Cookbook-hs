{- |
   Module      :   Cookbook.Project.Quill.Quil
   Copyright   :   (c) 2014 by Nate Pisarski
   License     :   BSD3
   Maintainer  :   nathanpisarski@gmail.com
   Stability   :   Stable
   Portability :   Portable (Cookbook
A library for reading simple databases. As what was originally a quirk of the library, database tables can be split up and still parsed as if they were a unit, giving it the ability to read the same table from multiple files in parallel. Quill supports a comment syntax, whitespace-insignificant parsing, and a full CRUD API. Quill will EVENTUALLY be replaced by Scribe2, but the planning for Scribe2 has not yet begun.
-}

module Cookbook.Project.Quill.Quill where 

import qualified Cookbook.Essential.Common         as Cm
import qualified Cookbook.Essential.Continuous     as Ct
import qualified Cookbook.Ingredients.Lists.Modify as Md
import qualified Cookbook.Ingredients.Lists.Access as Ac
import qualified Cookbook.Ingredients.Tupples.Look as Lk
import qualified Cookbook.Recipes.Sanitize         as Sn

-- | A table is a record of a name and the information within it.
data Table = Table {name :: String, info :: [(String,String)]}

-- | Sanitizes strings for Quill processing. Removes comments, newlines, and flattens it into a string.
prepare :: [String] -> String
prepare = (`Ct.splice` ("(*","*)")) . (`Sn.blacklist` "\n") . Cm.flt

-- | Splits a list into tupples.
tokenize :: [a] -> [(a,a)]
tokenize []  = []
tokenize [x] = []
tokenize (x:y:xs) = (x,y) : tokenize xs

-- | Returns the names of all tables in the file.
tblNames :: String -> [String]
tblNames x = Ct.before x '{' : tail (Md.surroundedBy (Sn.blacklist x " ")  ('}','{'))

-- | *Should not be used raw except for API programming. Really shouldn't be included, but too lazy to enumerate top-level declarations for selective export* Returns all entry lines from within tables in database.
headlessData :: String -> [[String]]
headlessData x = map (`Md.splitOn` ';') $ Md.surroundedBy x ('{','}')

-- | Returns all of the tokens from headlessData.
tokenLists :: [[String]] -> [[(String,String)]]
tokenLists =  map (map (\c -> (Ct.before c ':',Ct.after c ':')))

-- API functions
-- | Creates a listing of all tables in the file.
tables :: [String] -> [(String,[(String,String)])]
tables x = let y = prepare x in
   zip (tblNames y) $ tokenLists (headlessData y)

-- | Gets a particular table in the file, returning its key-value pairs.
getTable :: [(String,[(String,String)])] -> String -> [(String,String)]
getTable x c = Cm.flt $ Lk.lookList x c

-- | Gets the particular item within a table from a database.
lookUp :: [(String,[(String,String)])] -> (String,String) -> [String]
lookUp x (c,d) = (`Lk.lookList` d) $ getTable x c

-- | Creates a new table within the database.
createTable :: [(String,[(String,String)])] -> String -> [(String,[(String,String)])]
createTable x c = (c,[]) : x

-- | Removes an entire table from the database by name.
removeTable :: [(String,[(String,String)])] -> String -> [(String,[(String,String)])]
removeTable x c = [e | e@(d,_) <- x, d /= c]

-- | Removes a particular item from a table within a database.
removeItem :: [(String,[(String,String)])] -> (String,String) -> [(String,[(String,String)])]
removeItem [] _ = []
removeItem ((tbl,ite):re) (tb,it)
  | tbl == tb = (tbl,filter (\(c,d) -> c /= it) ite) : removeItem re (tb,it)
  | otherwise = (tbl,ite) : removeItem re (tb,it)

-- | Adds an item to a table within a database.
addItem :: [(String,[(String,String)])] -> String -> (String,String) -> [(String,[(String,String)])]
addItem [] _ _ = []
addItem ((a,b):c) tb it = if a == tb then (a,it:b) : addItem c tb it else (a,b) : addItem c tb it

-- | Changes an item within a table. The identifier will remain the same; only the rvalue will change.
changeItem :: [(String,[(String,String)])] -> (String,String) -> String -> [(String,[(String,String)])]
changeItem db (tb,it) c = addItem (removeItem db (tb,it)) tb (it,c)

-- | Basically a toString function for Quill tables. It turns data into a String format which can be parsed by the Quill parsing stack.
tableToString :: (String,[(String,String)]) -> String
tableToString (x,b) = x ++ "{" ++ Cm.flt (map detokenize b) ++ "}"
  where detokenize (a,b) = a ++ ":" ++ b ++ ";"
