module Cookbook.Project.Quill.Quill(tables) where

import qualified Cookbook.Essential.Continuous     as Ct
import qualified Cookbook.Ingredients.Lists.Modify as Md
import qualified Cookbook.Ingredients.Lists.Remove as Rm
import qualified Cookbook.Essential.Common         as Cm
import qualified Cookbook.Ingredients.Tupples.Look as Lk
-- There are three types of data in a proper Quill database.
-- - Lists
-- - templates
-- - implementations
-- There are also two kinds of templates and instantiations. This is why
--                  groups has been deprecated.
-- - Anonymous templates
-- - Anonymous instantiations
-- - Qualified templates
-- - Qualified instantiations

-- Custom data

-- All one lines, remove whitespace
prepare :: [String] -> String
prepare =  (++ "}"). unlines . map (flip Md.rm $  ' ')

--spcZip :: [a] -> [[a]] -> ([a],([a],[a]))
--spcZip x z = 
--getTables :: String -> [(String, [(String, String)])]
--getTables :: String -> [(String,[(String,String)])
getTables :: [Char] -> [([Char], [[Char]])]
getTables x = wrapped
  where names = Md.surroundedBy x ('}','{')
        items = Md.surroundedBy x ('{','}') 
        itemPairs = map ((flip Md.splitOn) ';') items
        wrapped = zip names itemPairs
        values = itemPairs

tokenize :: [a] -> [(a,a)]
tokenize [] = []
tokenize [x] = []
tokenize (x:y:xs) = (x,y) : tokenize xs

splitUp :: (String,[String]) -> (String,[(String,String)])
splitUp (a,b) = (a,tokenize (Cm.flt $ (map ((flip Md.splitOn) ':') b)))

tables :: [String] -> [(String, [(String, String)])]
tables = map splitUp . getTables . prepare

getTable :: [(String,[(String,String)])] -> String -> [[(String,String)]]
getTable a b = Lk.lookList a b
