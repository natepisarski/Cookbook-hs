--Cookbook.Essential.IO
--IO is the only Cookbook library to import System modules. As the name states, Cookbook.IO makes IO
--easier and less error-prone by wrapping common IO "gotchas" in a function.
module Cookbook.Essential.IO(filelines,prompt) where

import  System.IO
import System.Environment

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
