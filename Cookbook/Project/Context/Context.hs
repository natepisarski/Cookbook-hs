module Cookbook.Project.Context.Context(parseContexts,skim) where

import qualified Cookbook.Ingredients.Lists.Modify     as Md
import qualified Cookbook.Ingredients.Lists.Access     as Ac
import qualified Cookbook.Ingredients.Functional.Break as Br
import qualified Cookbook.Ingredients.Tupples.Look     as Lk
import qualified Cookbook.Ingredients.Lists.Replace    as Rp

import qualified Cookbook.Essential.IO                 as CIO

import System.Directory

-- Binary search capabilities
skim :: FilePath -> IO [(String,FilePath)]
skim x =
  do
    allFiles <- getDirectoryContents x
    return $ zip (allFiles) (map ((x++"/")++) allFiles)

--Helper functions
isGlobalCall x = and [x `Ac.contains` "{=",x `Ac.contains` "=}"]
getGlobalName x = Md.between x ('=','=')

-- Parsing
parseContexts :: [(String,String)] -> [String] -> [String] -- Returns a list of shell commands
parseContexts _ [] = []
parseContexts cmdList (line:lines)
  | isGlobalCall line = parseGlobal cmdList line (Br.filterBreak notEnd lines) : parseContexts cmdList (Br.removeBreak notEnd lines)
  | otherwise = parseContexts cmdList lines
  where notEnd = (\c -> not (c `Ac.contains` ("{"++getGlobalName line++"}")))

parseGlobal :: [(String,String)] -> String -> [String] -> String
parseGlobal cmdList header lines = (head $ Lk.lookList cmdList (getGlobalName header)) ++ " " ++ Rp.replace (unlines lines) ('\n',' ')
