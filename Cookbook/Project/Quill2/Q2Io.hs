{- |
   Module      :   Cookbook.Project.Quill2.Q2Io
   Copyright   :   (c) 2014 by Nate Pisarski
   License     :   BSD3
   Maintainer  :   nathanpisarski@gmail.com
   Stability   :   Stable
   Portability :   Portable (Cookbook)
Q2Io is a helper library for interacting with Quill files. It aids in the reading of and writing to files on the user's system.
-}

module Cookbook.Project.Quill2.Q2Io where

import qualified Cookbook.Project.Quill2.Q2Api as Q2Api

import qualified Cookbook.Essential.IO as Io

import Cookbook.Project.Quill2.Q2Prelude
import Cookbook.Project.Quill2.Q2Parse

import System.IO

-- | Read a Quill database from a file.
fromFile :: FilePath ->  IO [Quill]
fromFile x = do
  y <-  Io.filelines x
  return $ pFile y

-- | Send a Quill database into a parsable format in a file.
toFile :: FilePath -> [Quill] -> IO ()
toFile x f = writeFile x (unlines (map Q2Api.toString f))

