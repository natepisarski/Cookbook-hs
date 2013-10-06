--Cookbook.Essential.IO
--IO is the only Cookbook library to import System modules. As the name states, Cookbook.IO makes IO
--easier and less error-prone by wrapping common IO "gotchas" in a function.
module Cookbook.Essential.IO(filelines,prompt,inhome,getHomePath) where

import  System.IO
import System.Environment
import System.Directory

-- | Returns the lines of a file, wrapped in an IO.
filelines :: String -> IO ([String])
filelines x = do
  y <- openFile x ReadMode
  yc <- hGetContents y
  return (lines yc)

-- | Prompts the user for a string
prompt :: String -> IO (String)
prompt x = do
    putStr x
    hFlush stdout
    getLine

inhome :: String -> IOMode -> IO (Handle)
inhome x  c = do
  home <- getHomeDirectory
  openFile (home ++ x) c

getHomePath :: String -> IO (String)
getHomePath x = do
  home <- getHomeDirectory
  return (home ++ x)
