module Paths_maps (
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
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/Lucia/GitHub/main/maps/.cabal-sandbox/bin"
libdir     = "/Users/Lucia/GitHub/main/maps/.cabal-sandbox/lib/x86_64-osx-ghc-7.8.3/maps-0.1.0.0"
datadir    = "/Users/Lucia/GitHub/main/maps/.cabal-sandbox/share/x86_64-osx-ghc-7.8.3/maps-0.1.0.0"
libexecdir = "/Users/Lucia/GitHub/main/maps/.cabal-sandbox/libexec"
sysconfdir = "/Users/Lucia/GitHub/main/maps/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "maps_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "maps_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "maps_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "maps_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "maps_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
