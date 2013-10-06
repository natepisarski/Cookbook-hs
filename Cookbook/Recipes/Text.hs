--Cookbook.Recipes.Text
--A library with the sole purpose of manipulating lists of strings.

module Cookbook.Recipes.Text(linesBetween) where

import qualified Cookbook.Essential.Continuous as Ct
import qualified Cookbook.Ingredients.Tupples.Look as Lk
import qualified Cookbook.Ingredients.Lists.Access as Ac
import qualified Cookbook.Ingredients.Functional.Break as Br

linesBetween :: [String] -> (String,String) -> [String]
linesBetween x (c,d) = tail $ Br.filterBreak ( notContaining d) $ Br.removeBreak ( notContaining c) x
  where notContaining e l = not $ l `Ac.contains` e
