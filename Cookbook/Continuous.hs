{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Cookbook.Continuous(Continuous(..)) where

import Cookbook.Common
import Cookbook.Ingredients.Functional.Break

-- | Continuous provides an interface for function overloading. Everything automatically qualifies to be a constrained by class continuous, so no anotation is required in any type signatures.
class Continuous list part where
  
-- | After returns a sub-list after the first element or first occurence of a larger list.
  after :: list -> part -> list
  
-- | Before returns a sub-list before either the first occurence of an element or sublist.
  before :: list -> part -> list
  
instance (Eq a) => Continuous [a] a where
  after x c     = tail $ removeBreak (/=c) x
  before x c  = filterBreak (/=c) x
  
instance (Eq a) => Continuous [a] [a] where
  after [] _ = []
  after x c
    | take (length c) x == c = sub x ((length c))
    | otherwise = after (tail x) c

  before [] _ = []
  before x c
    | take (length c) x == c = []
    | otherwise = (head x) : before (tail x) c
