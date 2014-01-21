{-# LANGUAGE FlexibleInstances     #-}

{-# LANGUAGE MultiParamTypeClasses #-}

{- |
   Module      :   Cookbook.Essential.Continuous
   Copyright   :   (c) 2014 by Nate Pisarski
   License     :   BSD3
   Maintainer  :   nathanpisarski@gmail.com
   Stability   :   Unstable
   Portability :   Portable (Cookbook)

Library for overloading functions across lists and singular items, as well as tupples.
Somewhat abuses FlexibleInstance and Typeclasses.
-}
module Cookbook.Essential.Continuous where

import qualified Cookbook.Essential.Common             as Cm
import qualified Cookbook.Ingredients.Functional.Break as Br
import qualified Cookbook.Ingredients.Lists.Access     as Ac

-- | Classifies items that can be modified by either a list or item.
class Continuous list part where
  
  -- | Returns all elements after part.
  after :: list -> part -> list
  
  -- | Returns all elements after part.
  before :: list -> part -> list
  
  -- | Removes part from the list.
  delete :: list -> part -> list

instance (Eq a) => Continuous [a] a where
  after x c   = tail $ Br.removeBreak (/=c) x
  before x c  = Br.filterBreak        (/=c) x
  delete x c  = filter                (/=c) x
  
instance (Eq a) => Continuous [a] [a] where
  after [] _ = []
  after x c  = if Ac.isBefore x c then Cm.sub x (length c) else after (tail x) c

  before [] _ = []
  before x c  = if Ac.isBefore x c then [] else (head x) : before (tail x) c

  delete [] _ = []
  delete x c  = if not (x `Ac.contains` c) then x else  (before x c) ++ delete (after x c) c

-- | Classifies information which can be split by a tupple.
class Splicable a b where
  
  -- | Removes everything between the tupple's parameters, including the parameters themselves.
  splice :: a -> b -> a

instance (Eq a) => Splicable [a] (a,a) where
  splice ls (a,b) = before ls a ++ Cm.fromLast ((flip before) b) ls

instance (Eq a) => Splicable [a] ([a],[a]) where
  splice ls (a,b)  = before ls a ++ after (after ls a) b

-- | Classifies data which can be replaced.
class Replacable list repls where
  
  -- | Replaces part of list.
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

-- | Classifies data which can be removed from a list.
class Removable list toRm where
  -- | Remove data from a list.
  remove :: list -> toRm -> list

instance (Eq a) => Removable [a] a where
  remove x c = [y | y <- x, y /= c]

instance (Eq a) => Removable [a] [a] where
  remove [] _       = []
  remove x@(a:b) c  = if take (length c) x == c then remove (restart x) c else a : remove b c
    where restart d = Cm.sub d (length c)
