module Paths_cookbook (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [3,0,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/nate/.cabal/bin"
libdir     = "/home/nate/.cabal/lib/x86_64-linux-ghc-7.8.3/cookbook-3.0.0.0"
datadir    = "/home/nate/.cabal/share/x86_64-linux-ghc-7.8.3/cookbook-3.0.0.0"
libexecdir = "/home/nate/.cabal/libexec"
sysconfdir = "/home/nate/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "cookbook_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "cookbook_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "cookbook_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "cookbook_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "cookbook_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
