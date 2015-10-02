{- |
   Module      :   Cookbook.Essential.IO
   Copyright   :   (c) 2014 by Nate Pisarski
   License     :   BSD3
   Maintainer  :   nathanpisarski@gmail.com
   Stability   :   Stable
   Portability :   Non-Portable (Cookbook, Strict, Environment)

Library for completing common IO tasks, integrating with files and UNIX functions.
-}

module Cookbook.Essential.IO where

import qualified System.IO                         as LIO
import qualified System.IO.Strict                  as SIO
import qualified Cookbook.Essential.Common         as Cm
import qualified Cookbook.Essential.Continuous     as Ct
      
import System.Directory

-- | Return the lines of a file as a list of Strings.
filelines :: String -> IO [String]
filelines x = fmap lines $ LIO.openFile x LIO.ReadMode >>= SIO.hGetContents

-- | Prompts the user for keyboard input
prompt :: String -> IO String
prompt x = do
    putStr x
    LIO.hFlush LIO.stdout
    getLine

-- | Returns the path of a file in the user's home directory. 
inhome :: String -> LIO.IOMode -> IO LIO.Handle
inhome x c = fmap ((++x).(++"/")) getHomeDirectory >>= flip LIO.openFile c

-- | Pure. Returns the file name with the directory truncated.
filename :: String -> String
filename = Cm.fromLast (`Ct.before` '/')

-- | Pure. Returns the module name. That is, path to the file with the file cut off.
modulename :: String -> String
modulename = Cm.fromLast (`Ct.after` '/')
