module Cookbook.Project.Configuration.Configuration(conf) where
import qualified Cookbook.Ingredients.Lists.Modify as Md
import qualified Cookbook.Ingredients.Tupples.Look as Lk

conf :: [String] -> String -> String
conf x c = let configs = [let (d:f:_) = (Md.splitOn y ':') in (d,f)| y <- x, (length y) > 2, ':' `elem` y] in case (Lk.look configs c) of (Just f) -> f;(Nothing) -> []
