{- |
   Module      :   Cookbook.Project.Quill2.Q2Prelude
   Copyright   :   (c) 2014 by Nate Pisarski
   License     :   BSD3
   Maintainer  :   nathanpisarski@gmail.com
   Stability   :   Stable
   Portability :   Portable (Cookbook)
Q2Prelude is the entry-level module for Quill2. It defines the data types that define data, databases, and errors throughout the rest of Quill2.
-}

module Cookbook.Project.Quill2.Q2Prelude where
       
-- | The body of a table or list.
data Element a = List [a] | Table [(a,a)] deriving (Eq, Show)

-- | Helper type. Binds a name to a body.
type Quill = (String,Element String)

-- | Encapsulates errors in the quill database. Currently supports Missing elements and Multiple Instances.
data QuillStatus a = QuillSuccess a | QuillMissing String | QuillMultiple String deriving (Eq, Show)

-- | Safe way of adding items to a Quill database. Allows type-checking on Lists and Tables when manipulating Elements.
data QuillAddition = AList (String, String) | ATable (String, String, String) deriving (Eq, Show)
