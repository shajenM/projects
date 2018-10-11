{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_towerHanoi (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/sdevasse/myGitRepo/haskell/towerHanoi/.stack-work/install/x86_64-linux/lts-12.12/8.4.3/bin"
libdir     = "/home/sdevasse/myGitRepo/haskell/towerHanoi/.stack-work/install/x86_64-linux/lts-12.12/8.4.3/lib/x86_64-linux-ghc-8.4.3/towerHanoi-0.1.0.0-ERHrroXawnQ4yrBHhiTzm0"
dynlibdir  = "/home/sdevasse/myGitRepo/haskell/towerHanoi/.stack-work/install/x86_64-linux/lts-12.12/8.4.3/lib/x86_64-linux-ghc-8.4.3"
datadir    = "/home/sdevasse/myGitRepo/haskell/towerHanoi/.stack-work/install/x86_64-linux/lts-12.12/8.4.3/share/x86_64-linux-ghc-8.4.3/towerHanoi-0.1.0.0"
libexecdir = "/home/sdevasse/myGitRepo/haskell/towerHanoi/.stack-work/install/x86_64-linux/lts-12.12/8.4.3/libexec/x86_64-linux-ghc-8.4.3/towerHanoi-0.1.0.0"
sysconfdir = "/home/sdevasse/myGitRepo/haskell/towerHanoi/.stack-work/install/x86_64-linux/lts-12.12/8.4.3/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "towerHanoi_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "towerHanoi_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "towerHanoi_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "towerHanoi_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "towerHanoi_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "towerHanoi_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
