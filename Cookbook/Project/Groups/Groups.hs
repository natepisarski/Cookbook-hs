--Groups is a very small markup language with an inflexible syntax.
--Groups have data constructors and implementations.
--An example of a groups file can be found in the Examples directory of this repo
module Cookbook.Project.Groups.Groups(Constructor,construct,implement) where

import qualified Cookbook.Ingredients.Lists.Modify as Md
import qualified Cookbook.Ingredients.Tupples.Look as Lk
import qualified Cookbook.Essential.Continuous as Ct

type Constructor = (String,[String])

--Example: phone:number,name
-- | Construct a data constructor. Delmits using '_'
construct :: String -> Constructor
construct x = ((Ct.before x ':'),(Md.splitOn (Ct.after x ':') '_'))

--Example: phone:123-456-7899,Joe
-- | Using a list of constructors, implements an instantiation.
implement :: String -> [Constructor] -> [(String,String)]
implement x c = zip typ (Md.splitOn (Ct.after x ':') ',')
  where typ = case Lk.look c (Ct.before x ':') of (Just e) -> e;  _ -> error "Type undefined."
