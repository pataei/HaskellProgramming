{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_ch14 (
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

bindir     = "/Volumes/GoogleDrive/My Drive/OSU/PLbooks/Haskell_Programming_(EARLY_ACCESS)/Practice/ch14/.stack-work/install/x86_64-osx/lts-13.13/8.6.4/bin"
libdir     = "/Volumes/GoogleDrive/My Drive/OSU/PLbooks/Haskell_Programming_(EARLY_ACCESS)/Practice/ch14/.stack-work/install/x86_64-osx/lts-13.13/8.6.4/lib/x86_64-osx-ghc-8.6.4/ch14-0.1.0.0-3UYADJL4zarCGY0QMzdwGk"
dynlibdir  = "/Volumes/GoogleDrive/My Drive/OSU/PLbooks/Haskell_Programming_(EARLY_ACCESS)/Practice/ch14/.stack-work/install/x86_64-osx/lts-13.13/8.6.4/lib/x86_64-osx-ghc-8.6.4"
datadir    = "/Volumes/GoogleDrive/My Drive/OSU/PLbooks/Haskell_Programming_(EARLY_ACCESS)/Practice/ch14/.stack-work/install/x86_64-osx/lts-13.13/8.6.4/share/x86_64-osx-ghc-8.6.4/ch14-0.1.0.0"
libexecdir = "/Volumes/GoogleDrive/My Drive/OSU/PLbooks/Haskell_Programming_(EARLY_ACCESS)/Practice/ch14/.stack-work/install/x86_64-osx/lts-13.13/8.6.4/libexec/x86_64-osx-ghc-8.6.4/ch14-0.1.0.0"
sysconfdir = "/Volumes/GoogleDrive/My Drive/OSU/PLbooks/Haskell_Programming_(EARLY_ACCESS)/Practice/ch14/.stack-work/install/x86_64-osx/lts-13.13/8.6.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "ch14_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "ch14_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "ch14_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "ch14_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ch14_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "ch14_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
