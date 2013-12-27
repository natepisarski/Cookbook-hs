{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Cookbook.Ingredients.Lists.Splice(Splicable(..)) where
import qualified Cookbook.Essential.Continuous as Ct
import qualified Cookbook.Essential.Common     as Cm

class Splicable a b where
  splice :: a -> b -> a

instance (Eq a) => Splicable [a] (a,a) where
  splice ls (a,b) = Ct.before ls a ++ Cm.fromLast ((flip Ct.before) b) ls

instance (Eq a) => Splicable [a] ([a],[a]) where
  splice ls (a,b)  = Ct.before ls a ++ Ct.after (Ct.after ls a) b
