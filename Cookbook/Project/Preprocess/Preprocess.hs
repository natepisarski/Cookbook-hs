--Cookbook.Project.Preprocess.Preprocess
--A library for implementing preprocess syntax, as defined in the .std

module Cookbook.Project.Preprocess.Preprocess where

import qualified Cookbook.Ingredients.Lists.Modify  as Md
import qualified Cookbook.Essential.Common          as Cm
import qualified Cookbook.Essential.Continuous      as Ct

makeParams :: String -> [(String,String)]
makeParams x = (flip zip) (repeat lastPart) $ Md.splitOn firstPart '|'
  where (firstPart:lastPart:[]) = Md.splitOn x '_'

sanitize :: String -> String
sanitize x = Ct.splice x ('$','$')

gPL :: [String] -> [(String,String)]
gPL x = Cm.flt $ map makeParams sanitized
  where sanitized = Ct.remove (map (\c -> if (length c) > 3 then (sanitize c) else "") x) ""
