module Cookbook.Project.Configuration.Configuration(conf) where
import Cookbook.Ingredients.Lists.Access
import Cookbook.Ingredients.Lists.Modify
import Cookbook.Ingredients.Tupples.Look

conf :: [String] -> String -> String
conf x c = let configs = [let (d:f:_) = (splitOn y ':') in (d,f)| y <- x, (length y) > 2, ':' `elem` y] in case (look configs c) of (Just f) -> f
                                                                                                                                    (Nothing) -> []
