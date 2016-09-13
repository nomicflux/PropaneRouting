module Paths_blog_tutorial (
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

bindir     = "/Users/demouser/Library/Haskell/bin"
libdir     = "/Users/demouser/Library/Haskell/ghc-7.10.3-x86_64/lib/blog-tutorial-0.1.0.0"
datadir    = "/Users/demouser/Library/Haskell/share/ghc-7.10.3-x86_64/blog-tutorial-0.1.0.0"
libexecdir = "/Users/demouser/Library/Haskell/libexec"
sysconfdir = "/Users/demouser/Library/Haskell/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "blog_tutorial_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "blog_tutorial_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "blog_tutorial_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "blog_tutorial_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "blog_tutorial_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
