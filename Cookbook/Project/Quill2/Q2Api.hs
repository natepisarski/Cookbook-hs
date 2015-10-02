{- |
   Module      :   Cookbook.Project.Quill2.Q2Api
   Copyright   :   (c) 2014 by Nate Pisarski
   License     :   BSD3
   Maintainer  :   nathanpisarski@gmail.com
   Stability   :   Stable
   Portability :   Portable (Cookbook)
Q2Api is the user-facing part of Quill. It has all the functions necessar to Create, Read, Update, and Delete information from the database, and turn a database back into a Quill-readable string.
-}

module Cookbook.Project.Quill2.Q2Api where

import qualified Cookbook.Ingredients.Tupples.Look as Lk
import qualified Cookbook.Essential.Common         as Cm
import qualified Cookbook.Essential.Continuous     as Ct
import qualified Cookbook.Ingredients.Lists.Access as Ac

import Cookbook.Project.Quill2.Q2Prelude

-- | Get the name of a Quill.
getQuillName :: Quill -> String
getQuillName = fst

-- | Return the element of the Quill, specifically useful for lists.
getQuillBody :: Quill -> Element String
getQuillBody = snd

-- | Find a quill in the database by name, returning it or a possible error type.
getQuill :: [Quill] -> String -> QuillStatus Quill
getQuill [] c = QuillMissing c
getQuill (x:xs) c
  | Ac.count (map getQuillName (x:xs)) c > 1 = QuillMultiple c
  | getQuillName x  == c = QuillSuccess x
  | otherwise = getQuill xs c

-- | Look up the value of a Quill TABLE. Will produce an error on a list.
lookUp :: [Quill] -> (String, String) -> QuillStatus String
lookUp x (a,b) = case getQuill x a of
  (QuillSuccess (_,d)) -> case d of
    (List _) -> error "Cannot look up the value of a list."
    (Table f) -> let c = Lk.lookList f b in if null c|| (length c > 1) then QuillMissing b else QuillSuccess $ head c
  (QuillMissing _)  -> QuillMissing a
  (QuillMultiple _) -> QuillMultiple a

-- | Remove a quill from the database by name.
removeQuill :: [Quill] -> String -> [Quill]
removeQuill [] _ = []
removeQuill ((x,y):xs) c
  | x == c = removeQuill xs c -- See [Q2N1]
  | otherwise = (x,y) : removeQuill xs c

-- | Remove an item from a Quill within a database. Works aggressively, meaning it removes all copies to help sanitize QuillMultiples out. th
removeItem :: [Quill] -> (String, String) -> QuillStatus [Quill]
removeItem x (a,b) = case getQuill x a of
  QuillSuccess (c,j) -> case j of
    (Table d) -> QuillSuccess $ (c, Table [(y,t) | (y,t) <- d, y /= b]) : removeQuill x a
    (List d) -> QuillSuccess  $ (c,List $ Ct.remove d c) : removeQuill x a
  QuillMissing _ -> QuillMissing a
  QuillMultiple _ -> QuillMultiple a

-- | Adds a Quill databse to the file.
addQuill :: [Quill] -> Quill -> [Quill]
addQuill x c = c : x

-- | Add a QuillAddition to the databse. QuillAddition is a safe encapsulation of list and table values.
addItem :: [Quill] -> QuillAddition -> QuillStatus [Quill]
addItem x qa = case getQuill x a of
  QuillSuccess (y, ys) -> case ys of
    (Table d) -> case qa of
      (ATable (_,b,c)) -> QuillSuccess $ (y,Table $ (b,c) : d) : removeQuill x a
      (AList _ ) -> error $ "$ Type Mismatch! Attempted to add a List type to Table in table " ++ show qa
    (List d)  -> case qa of
      (AList (_,b)) -> QuillSuccess $ (y, List (b:d)) : removeQuill x a
      (ATable _) -> error $ "Type Mismatch! Attempted to add Table type to List in table " ++ show qa
  QuillMultiple _ -> QuillMultiple a
  QuillMissing  _ -> QuillMissing a --M
  where
    a = case qa of
      (ATable (g,_,_)) -> g
      (AList  (g,_))   -> g

-- | Map a Quill function.
qMap :: QuillStatus [Quill] -> ([Quill] -> QuillStatus [Quill]) -> QuillStatus [Quill]
qMap c f = case c of
  (QuillSuccess a) ->  f a
  _ -> c

-- | Change an item within the database using a Quill addition. Wrapper of addItem and removeItem.
changeItem :: [Quill] -> QuillAddition -> QuillStatus [Quill]
changeItem x y = case y of
                      (ATable (a,b,_)) ->  qMap (removeItem x (a,b)) (`addItem` y)
                      (AList  (a,b))   ->  qMap (removeItem x (a,b)) (`addItem` y)
                 

-- | Turn a Quill table into a string.
toString :: Quill -> String
toString (nm,typ) = case typ of
  (List a)  -> Cm.flt ["list(",  nm, "){", Cm.flt (map (++ ";") a),"}"]
  (Table a) -> Cm.flt ["table(", nm, "){", Cm.flt (stringify a),   "}"]
  where
    stringify [] = []
    stringify ((a,b):xs) = Cm.flt [a,":",b,";"]  : stringify xs
