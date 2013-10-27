--Cookbook.Project.Scribe
--Scribe is a tool for communicating with SCRIBE databases.
module Cookbook.Project.Scribe.Scribe(
  Frame(..),Column,Row,Table,Database,
  isRow,isColumn,isFrame,isTable,
  makeName,
  parseFrame,parseColumn,parseRow,parseTable,
  readColumn,readRow,readTable,readDatabase,
  getTable,getRow,getColumn,getFrame,
  qGetRow,qGetColumn,qGetFrame) where

import qualified Cookbook.Essential.Common             as Cm
import qualified Cookbook.Essential.Continuous         as Ct
import qualified Cookbook.Ingredients.Lists.Modify     as Md
import qualified Cookbook.Ingredients.Lists.Access     as Ac
import qualified Cookbook.Ingredients.Tupples.Look     as Lk
import qualified Cookbook.Ingredients.Functional.Break as Br
import qualified Cookbook.Recipes.Sanitize             as St

--Data
data Frame = Frame {keyval :: String, entry :: String} | Promise {keyval :: String, link :: String}  deriving (Eq,Show)

--Type synonyms
type Column = (String,[Frame])
type Row = (String,[Column])
type Table = (String,[Row])
type Database = [Table]

--Standard information
isRow     x = and [x `Ac.contains` "row",    length x > 4]
isColumn  x = and [x `Ac.contains` "column", length x > 4]
isFrame   x = and $ map ((flip elem) x) "<>"
isTable   x = and [x `Ac.contains` "table",length x > 4]

makeName :: String -> String -> String
makeName a b = St.blacklist (Ct.after a b) "{ "

--Parsing
parseFrame :: String -> Frame
parseFrame x | not $ isFrame x = error (x ++ " was passed to parseFrame, but does not conform to standard.")
parseFrame x
  | ('-','-') `Ac.surrounds` inner = Promise {keyval = Md.between inner ('-','-'), link = Ct.after x '>'}
  | otherwise = Frame {keyval = inner, entry = Ct.after x '>'} -- Surrounded in <-->? It's a link.
  where inner = Md.between x ('<','>')

parseColumn :: (String,[String]) -> Column -- Expects the column declaration, and the relevant lines.
parseColumn (a,b) = (makeName a "column ",map parseFrame b)

parseRow :: (String,[(String,[String])]) -> Row
parseRow (a,b) = (makeName a "row ",map parseColumn b)

parseTable :: (String,[(String,[(String,[String])])]) -> Table
parseTable (a,b) = (makeName a "table ",map parseRow b)

--Reading
readColumn :: [String] -> (String,[String])
readColumn [] = error "Column either does not exist or does not conform to standards."
readColumn (x:xs) | not  $ isColumn x = readColumn xs
readColumn (x:xs) = (x,Md.linesBetween (x:xs) ("{","}"))

readRow :: [String] -> (String,[(String,[String])])
readRow [] = error "Row either does not exist or does not conform to standards."
readRow (x:xs) | not $ isRow x = readRow xs
readRow (x:xs) = (x, map readColumn splitup)
  where splitup = Br.splitBool (\c -> not $ c `Ac.contains` "}") rowScope
        rowScope = Md.linesBetween (x:xs) ("{","}}")

readTable :: [String] -> (String,[(String,[(String,[String])])])
readTable [] = error "Table either does not exist or does not conform to standards."
readTable (x:xs) | not $ isTable x = readTable xs
readTable (x:xs) = (x,map readRow splitup)
  where splitup = Br.splitBool (\c -> not $ c `Ac.contains` "}}") tableScope;
        tableScope = Md.linesBetween (x:xs) ("{","}}}")

--Final functions
readDatabase :: [String] -> Database
readDatabase (x:xs)
  | isTable x = parseTable (readTable (x:xs)) : readDatabase (Ct.after xs "}}}")
  | otherwise = readDatabase xs

--Accessors
getTable :: String -> Database -> Table
getTable a b = (a,(head $ Lk.lookList b a))

getRow :: (String,String) -> Database -> Row
getRow (a,b) c = (b,head $ Lk.lookList (snd (getTable a c)) b)

getColumn :: (String,String,String) -> Database -> Column
getColumn (a,b,c) d = (c,head $ Lk.lookList (snd (getRow (a,b) d)) c)

getFrame :: (String,String,String,String) -> Database -> Frame
getFrame (a,b,c,d) e = let frames = (snd $ getColumn (a,b,c) e) in head $ Lk.lookList (zip (map keyval frames) frames) d

--Shorthand accessors
qGetRow :: String -> Database -> Row
qGetRow x z= getRow (a,b) z
  where (a,b) = let (f:g:[]) = Md.splitOn x '.' in (f,g)

qGetColumn :: String -> Database -> Column
qGetColumn x z = getColumn (a,b,c) z
  where (a,b,c) = let (f:g:h:[]) = Md.splitOn x '.' in (f,g,h)

qGetFrame :: String -> Database -> Frame
qGetFrame x z = getFrame (a,b,c,d) z
  where (a,b,c,d) = let (f:g:h:j:[]) = Md.splitOn x '.' in (f,g,h,j)
