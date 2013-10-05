{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

--Cookbook.Essential.Continuous
--Continuous creates a generic overloaded interface to modify lists.
--Making use of generic typeclass constructors, a constraint to the Continuous
--typeclass (and thus, the LANGUAGE Pragmas) is not needed.
module Cookbook.Essential.Continuous(Continuous(..)) where

import qualified Cookbook.Essential.Common as Cm
import qualified Cookbook.Ingredients.Functional.Break as Br
import qualified Cookbook.Ingredients.Lists.Access as Ac

-- | Continuous provides an interface for function overloading. Everything automatically qualifies to be a constrained by class continuous, so no anotation is required in any type signatures.
class Continuous list part where
  
-- | After returns a sub-list after the first element or first occurence of a larger list.
  after :: list -> part -> list
  
-- | Before returns a sub-list before either the first occurence of an element or sublist.
  before :: list -> part -> list

-- | Remove an item from a list, when used on an element, it works the same as rm.
  delete :: list -> part -> list

instance (Eq a) => Continuous [a] a where
  after x c     = tail $ Br.removeBreak (/=c) x
  before x c  = Br.filterBreak (/=c) x
  delete x c  = filter (/= c) x
  
instance (Eq a) => Continuous [a] [a] where
  after [] _ = []
  after x c = if Ac.isBefore x c then Cm.sub x (length c) else after (tail x) c

  before [] _ = []
  before x c = if Ac.isBefore x c then [] else (head x) : before (tail x) c

  delete [] _ = []
  delete x c = if not (x `Ac.contains` c) then x else  (before x c) ++ delete (after x c) c
