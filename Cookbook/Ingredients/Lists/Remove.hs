--Cookbook.Ingredients.Lists.Remove
--Remove is an OGI for removing parts of lists.

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Cookbook.Ingredients.Lists.Remove(Removable(..)) where

import Cookbook.Essential.Common             as Cm
import Cookbook.Ingredients.Functional.Break as Br
import Cookbook.Ingredients.Lists.Modify     as Md

class Removable list toRm where

  remove :: list -> toRm -> list

instance (Eq a) => Removable [a] a where
  remove x c = [y | y <- x, y /= c]

instance (Eq a) => Removable [a] [a] where
  remove [] _ = []
  remove x@(a:b) c = if take (length c) x == c then remove (restart x) c else a : remove b c
    where restart d = Cm.sub d (length c)

instance (Eq a) => Removable [a] (a,a) where
  remove [] _ = []
  remove (x:xs) (a,b) = if and [x == a, b `elem` xs] then remove (tail (Br.removeBreak (/=b) xs)) (a,b) else x : remove xs (a,b)
