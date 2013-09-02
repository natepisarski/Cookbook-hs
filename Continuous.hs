{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Cookbook.Continuous(Continuous(..)) where

import Cookbook.Common
import Cookbook.Ingredients.Functional.Break

class Continuous list part where
  after :: list -> part -> list
  before :: list -> part -> list
  
instance (Eq a) => Continuous [a] a where
  after x c = tail $ removeBreak (/=c) x
  before x c = filterBreak (/=c) x
  
instance (Eq a) => Continuous [a] [a] where
  after [] _ = []
  after x c
    | take (length c) x == c = sub x ((length c))
    | otherwise = after (tail x) c

  before [] _ = []
  before x c
    | take (length c) x == c = []
    | otherwise = (head x) : before (tail x) c
