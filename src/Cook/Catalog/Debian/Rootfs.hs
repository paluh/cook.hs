module Cook.Catalog.Debian.Rootfs (buildRootfs, buildRootfsFromCache) where

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

packages = [ "init"
           , "ifupdown"
           , "locales"
           , "dialog"
           , "isc-dhcp-client"
           , "netbase"
           , "net-tools"
           , "iproute"
           , "openssh-server"
           ]

runDebootrap suite path =
  run (debootstrap defaults{include=packages} suite path Nothing)

buildRootfs :: Suite -> FilePath -> Recipe FilePath
buildRootfs suite path = withRecipeName "Debian.Rootfs.BuildRootfs" $ do
  runDebootrap suite path
  return path

type CacheDir = FilePath

buildRootfsFromCache :: Suite -> CacheDir -> RootFs -> Recipe RootFs
buildRootfsFromCache suite cache path = withRecipeName "Debian.Rootfs.BuildRootfsFromCache" $ do
  let suiteCache = cache </> (addTrailingPathSeparator . showSuite $ suite)
  liftIO $ createDirectoryIfMissing True cache
  suiteCached <- liftIO $ doesDirectoryExist suiteCache
  unless suiteCached (void (runDebootrap suite suiteCache))
  runProc "rsync" ["-H", "-a", suiteCache, path]
  return path
