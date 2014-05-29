{- |
   Module      :   Cookbook.Project.Preprocess.Preprocess
   Copyright   :   (c) 2014 by Nate Pisarski
   License     :   BSD3
   Maintainer  :   nathanpisarski@gmail.com
   Stability   :   Stable
   Portability :   Portable (Cookbook
A library for preprocessing information using replacing sweeps. Supports inline comments. Does not currently support whitespace-insignificant parsing.
-}

module Cookbook.Project.Preprocess.Preprocess where

import qualified Cookbook.Ingredients.Lists.Modify  as Md
import qualified Cookbook.Essential.Common          as Cm
import qualified Cookbook.Essential.Continuous      as Ct

-- | Binds possibly multiple inputs on one line to one input. General syntax is: inp1|inp2_out
makeParams :: String -> [(String,String)]
makeParams x = flip zip (repeat lastPart) $ Md.splitOn firstPart '|'
  where (firstPart:lastPart:[]) = Md.splitOn x '_'

-- | Sanitizes the strings before parsing.
sanitize :: String -> String
sanitize x = Ct.splice x ('$','$')

-- | Generate Program Language. Generates a list of input-output pairs to be replaced.
gPL :: [String] -> [(String,String)]
gPL x = Cm.flt $ map makeParams sanitized
  where sanitized = Ct.remove (map (\c -> if length c > 3 then sanitize c else "") x) ""
