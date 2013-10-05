{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

--Cookbook.Ingredients.Lists.Replace
--Replace, much alike Cookbook.Essential.Continuous, is a generic overloaded typeclass for replacing parts of lists.
--It is overloaded to work with a list and elements, a list and a list of elements, and a list and a list of elements which are lists, and a lists of a lists of elements which are lists.

module Cookbook.Ingredients.Lists.Replace(Replacable(..)) where
import qualified Cookbook.Essential.Common as Cm

-- | Provides a generic interface to replacable lists.
class Replacable list repls where

-- | Transforms lists of any type.
  replace  :: list -> repls -> list

instance (Eq a) => Replacable [a] (a,a) where
  replace lst (on,tw) = map (\c -> if c == on then tw else c) lst
  
instance (Eq a) => Replacable [a] [(a,a)] where
  replace x c = Cm.apply (map (flip replace) c) x
  
instance (Eq a) => Replacable [a] ([a],[a]) where
  replace [] _ = []
  replace lst (on,tw) 
    | (take (length on) lst) == on = tw ++ replace (Cm.sub lst (length on)) (on,tw)
    | otherwise = (head lst) : replace (tail lst) (on,tw)

instance (Eq a) => Replacable [a] [([a],[a])] where
  replace x c = Cm.apply (map (flip replace) c) x

