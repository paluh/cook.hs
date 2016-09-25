module Cook.Catalog.Arch.Pacman (
    pacmanProvider
  ) where

import Control.Monad
import Data.List.NonEmpty
import Data.Semigroup

import Cook.Recipe
import Cook.Recipe.Provider.Pkg

pacman :: NonEmpty String -> Step
pacman args = proc "pacman" $ toList $ ["--quiet", "--noconfirm"] <> args

upgradePackages :: Recipe ()
upgradePackages = withRecipeName "Arch.Pacman.UpgradePackages" $ do
    run $ pacman ["-Syu"]

installPackages :: NonEmpty String -> Recipe ()
installPackages pkgs = withRecipeName "Arch.Pacman.InstallPackages" $ do
    run $ pacman $ ["--needed", "-S"] <> pkgs

isPackageInstalled :: String -> Recipe Bool
isPackageInstalled ""  = error "isPackageInstalled: empty package name"
isPackageInstalled pkg = withRecipeName "Arch.Pacman.IsPackageInstalled" $ do
    err <- withoutError $ runRead $ pacman ["-Q", pkg]
    either (const $ return False) (const $ return True) err

pacmanProvider :: PkgProvider
pacmanProvider = PkgProvider upgradePackages isPackageInstalled installPackages
