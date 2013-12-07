--Cookbook.Essential.IO
--IO is the only Cookbook library to import System modules. As the name states, Cookbook.IO makes IO
--easier and less error-prone by wrapping common IO "gotchas" in a function.
module Cookbook.Essential.IO(filelines,prompt,inhome,getHomePath,filename,modulename) where

import qualified System.IO as LIO
import  qualified System.IO.Strict as SIO
import qualified Cookbook.Essential.Continuous as Ct
import qualified Cookbook.Ingredients.Lists.Modify as Md
import System.Environment
import System.Directory

-- | Returns the lines of a file, wrapped in an IO.
filelines :: String -> IO ([String])
filelines x = do
  y <- LIO.openFile x LIO.ReadMode
  yc <- SIO.hGetContents y
  return (lines yc)

-- | Prompts the user for a string
prompt :: String -> IO (String)
prompt x = do
    putStr x
    LIO.hFlush LIO.stdout
    getLine

inhome :: String -> LIO.IOMode -> IO (LIO.Handle)
inhome x  c = do
  home <- getHomeDirectory
  LIO.openFile (home ++ x) c

getHomePath :: String -> IO (String)
getHomePath x = do
  home <- getHomeDirectory
  return (home ++ x)

filename :: String -> String
filename = Md.rev . ((flip Ct.before) '/') . Md.rev

modulename :: String -> String
modulename = Md.rev . ((flip Ct.after) '/') . Md.rev
