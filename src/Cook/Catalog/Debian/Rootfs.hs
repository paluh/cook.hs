module Cook.Catalog.Debian.Rootfs where

import Cook.Catalog.Debian.Debootstrap (debootstrap, defaults, include, Options, variant, Variant(Minbase))
import Cook.Catalog.Debian.Suite (showSuite, stable, Suite)
import Cook.Recipe (Recipe, run, withRecipeName, runProc)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import System.FilePath ((</>), addTrailingPathSeparator)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (unless, void)
import Data.List (isSuffixOf)
import Data.Monoid ((<>))

type RootFs = FilePath

buildRootfs :: Suite -> FilePath -> Recipe FilePath
buildRootfs suite path = withRecipeName "Debian.Rootfs.BuildRootfs" $ do
  run (debootstrap defaults suite path Nothing)
  return path

type CacheDir = FilePath

buildRootfsFromCache :: Suite -> CacheDir -> RootFs -> Recipe RootFs
buildRootfsFromCache suite cache path = withRecipeName "Debian.Rootfs.BuildRootfsFromCache" $ do
  let suiteCache = cache </> (addTrailingPathSeparator . showSuite $ suite)
  liftIO $ createDirectoryIfMissing True cache
  suiteCached <- liftIO $ doesDirectoryExist suiteCache
  unless suiteCached (void (buildRootfs suite suiteCache))
  runProc "rsync" ["-H", "-a", suiteCache, path]
  liftIO $ print path
  return path
