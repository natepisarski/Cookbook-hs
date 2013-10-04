{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Cookbook.Ingredients.Lists.Replace(Replacable(..)) where
import Cookbook.Common

class Replacable list repls where
  replace  :: list -> repls -> list

instance (Eq a) => Replacable [a] (a,a) where
  replace lst (on,tw) = map (\c -> if c == on then tw else c) lst
  
instance (Eq a) => Replacable [a] [(a,a)] where
  replace x c = apply (map (flip replace) c) x
  
instance (Eq a) => Replacable [a] ([a],[a]) where
  replace [] _ = []
  replace lst (on,tw)
    | (take (length on) lst) == on = tw ++ replace (sub lst (length on)) (on,tw)
    | otherwise = (head lst) : replace (tail lst) (on,tw)

instance (Eq a) => Replacable [a] [([a],[a])] where
  replace x c = apply (map (flip replace) c) x

