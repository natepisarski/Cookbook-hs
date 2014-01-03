{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

--Cookbook.Essential.Continuous
--Continuous is the most important Cookbook library. It defines Overloaded Generic Interfaces
-- (refered to as OGI's throughout the library) for working on two generic types of data, usually
--   lists and items.
module Cookbook.Essential.Continuous where

import qualified Cookbook.Essential.Common             as Cm
import qualified Cookbook.Ingredients.Functional.Break as Br
import qualified Cookbook.Ingredients.Lists.Access     as Ac

class Continuous list part where
  after :: list -> part -> list
  before :: list -> part -> list
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

-- Spliceable
class Splicable a b where
  splice :: a -> b -> a

instance (Eq a) => Splicable [a] (a,a) where
  splice ls (a,b) = before ls a ++ Cm.fromLast ((flip before) b) ls

instance (Eq a) => Splicable [a] ([a],[a]) where
  splice ls (a,b)  = before ls a ++ after (after ls a) b

-- Replacable
class Replacable list repls where
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

-- Removable
class Removable list toRm where
  remove :: list -> toRm -> list

instance (Eq a) => Removable [a] a where
  remove x c = [y | y <- x, y /= c]

instance (Eq a) => Removable [a] [a] where
  remove [] _       = []
  remove x@(a:b) c  = if take (length c) x == c then remove (restart x) c else a : remove b c
    where restart d = Cm.sub d (length c)

instance (Eq a) => Removable [a] (a,a) where
  remove [] _ = []
  remove (x:xs) (a,b) = if and [x == a, b `elem` xs] then remove (tail (Br.removeBreak (/=b) xs)) (a,b) else x : remove xs (a,b)
