{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_blackjack_trainer (
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

bindir     = "/home/iizuki/.cabal/bin"
libdir     = "/home/iizuki/.cabal/lib/x86_64-linux-ghc-8.10.4/blackjack-trainer-0.1.0.0-inplace-blackjack-trainer"
dynlibdir  = "/home/iizuki/.cabal/lib/x86_64-linux-ghc-8.10.4"
datadir    = "/home/iizuki/.cabal/share/x86_64-linux-ghc-8.10.4/blackjack-trainer-0.1.0.0"
libexecdir = "/home/iizuki/.cabal/libexec/x86_64-linux-ghc-8.10.4/blackjack-trainer-0.1.0.0"
sysconfdir = "/home/iizuki/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "blackjack_trainer_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "blackjack_trainer_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "blackjack_trainer_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "blackjack_trainer_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "blackjack_trainer_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "blackjack_trainer_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
