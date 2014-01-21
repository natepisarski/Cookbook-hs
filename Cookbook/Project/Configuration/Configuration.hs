{- |
   Module      :   Cookbook.Project.Configuration.Configuration
   Copyright   :   (c) 2014 by Nate Pisarski
   License     :   BSD3
   Maintainer  :   nathanpisarski@gmail.com
   Stability   :   Stable
   Portability :   Portable (Cookbook)
Configuration is a library for simple configuration files. It's pretty fragile, but it's been used for some pretty majorly used scripts, namely espion. The lvalue becomes the fst of a name-value pair, and the rvalue becomes the second, with the syntax: lvalue : rvalue\n
-}

module Cookbook.Project.Configuration.Configuration where
import qualified Cookbook.Ingredients.Lists.Modify as Md
import qualified Cookbook.Ingredients.Tupples.Look as Lk

-- | Read the lines of a configuration file, query it, and return an answer to the query.
conf :: [String] -> String -> String
conf x c = let configs = [let (d:f:_) = (Md.splitOn y ':') in (d,f)| y <- x, (length y) > 2, ':' `elem` y] in case (Lk.look configs c) of (Just f) -> f;(Nothing) -> []
