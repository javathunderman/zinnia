{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_state_hw (
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

bindir     = "/home/home/src/cmsc433/state/.stack-work/install/x86_64-linux-tinfo6/0a345aaccdc8e18da4b2d88344deebea439ba1fc4b7ff2ab742c0ef4f0e0d460/8.10.7/bin"
libdir     = "/home/home/src/cmsc433/state/.stack-work/install/x86_64-linux-tinfo6/0a345aaccdc8e18da4b2d88344deebea439ba1fc4b7ff2ab742c0ef4f0e0d460/8.10.7/lib/x86_64-linux-ghc-8.10.7/state-hw-0.1.0.0-J84PUfwtkWa7vtc2xpGECO"
dynlibdir  = "/home/home/src/cmsc433/state/.stack-work/install/x86_64-linux-tinfo6/0a345aaccdc8e18da4b2d88344deebea439ba1fc4b7ff2ab742c0ef4f0e0d460/8.10.7/lib/x86_64-linux-ghc-8.10.7"
datadir    = "/home/home/src/cmsc433/state/.stack-work/install/x86_64-linux-tinfo6/0a345aaccdc8e18da4b2d88344deebea439ba1fc4b7ff2ab742c0ef4f0e0d460/8.10.7/share/x86_64-linux-ghc-8.10.7/state-hw-0.1.0.0"
libexecdir = "/home/home/src/cmsc433/state/.stack-work/install/x86_64-linux-tinfo6/0a345aaccdc8e18da4b2d88344deebea439ba1fc4b7ff2ab742c0ef4f0e0d460/8.10.7/libexec/x86_64-linux-ghc-8.10.7/state-hw-0.1.0.0"
sysconfdir = "/home/home/src/cmsc433/state/.stack-work/install/x86_64-linux-tinfo6/0a345aaccdc8e18da4b2d88344deebea439ba1fc4b7ff2ab742c0ef4f0e0d460/8.10.7/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "state_hw_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "state_hw_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "state_hw_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "state_hw_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "state_hw_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "state_hw_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
