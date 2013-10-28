--Cookbook.Project.Preprocess.Preprocess
--A library for implementing preprocess syntax, as defined in the .std

module Cookbook.Project.Preprocess.Preprocess(makeParams,gPL) where

import qualified Cookbook.Ingredients.Lists.Replace as Rp
import qualified Cookbook.Ingredients.Lists.Modify  as Md
import qualified Cookbook.Ingredients.Lists.Remove  as Rm

import qualified Cookbook.Essential.Common as Cm

makeParams :: String -> [(String,String)]
makeParams x = (flip zip) (repeat lastPart) $ Md.splitOn firstPart '|'
  where (firstPart:lastPart:[]) = Md.splitOn x '_'

sanitize :: String -> String
sanitize x = Rm.remove x ('$','$')

gPL :: [String] -> [(String,String)]
gPL x = Cm.flt $ map makeParams sanitized
  where sanitized = Rm.remove (map (\c -> if (length c) > 3 then (sanitize c) else "") x) ""
