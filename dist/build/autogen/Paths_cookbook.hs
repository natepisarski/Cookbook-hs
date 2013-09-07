module Paths_cookbook (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/usr/local/bin"
libdir     = "/usr/local/lib/cookbook-0.1.0.0/ghc-7.6.2"
datadir    = "/usr/local/share/cookbook-0.1.0.0"
libexecdir = "/usr/local/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "cookbook_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "cookbook_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "cookbook_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "cookbook_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
