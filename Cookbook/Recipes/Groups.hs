--Groups is a very small markup language with an inflexible syntax.
--Groups have data constructors and implementations.
--An example of a groups file can be found in the Examples directory of this repo
module Cookbook.Recipes.Groups() where

import Cookbook.Ingredients.Lists.Modify
import Cookbook.Ingredients.Lists.Access
import Cookbook.Ingredients.Tupples.Look
import Cookbook.Recipes.Sanitize
import Cookbook.Continuous

type Constructor = (String,[String])

--Example: phone:number,name
-- | Construct a data constructor. Delmits using '_'
construct :: String -> Constructor
construct x = ((before x ':'),(splitOn (after x ':') '_'))

--Example: phone:123-456-7899,Joe
-- | Using a list of constructors, implements an instantiation.
implement :: String -> [Constructor] -> [(String,String)]
implement x c = zip typ (splitOn (after x ':') ',')
  where typ = case look c (before x ':') of (Just e) -> e;  _ -> error "Type undefined."
