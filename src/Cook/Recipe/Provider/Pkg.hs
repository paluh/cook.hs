module Cook.Recipe.Provider.Pkg (
    PkgProvider
  , nullProvider
  ) where

import Data.List.NonEmpty

import Cook.Recipe

data PkgProvider =
    { upgradePackages    :: Recipe ()
    , isPackageInstalled :: String -> Recipe Bool
    , installPackages    :: NonEmpty String -> Recipe ()
    }

requirePackages :: NonEmpty String -> Recipe ()
requirePackages pkgs = withRecipeName "Provider.Pkg.RequirePackages" $ do
    -- TODO: getProvider
    missingPkgs <- filterM (fmap not . isPackageInstalled) $ toList pkgs
    case missingPkgs of
        [] -> return ()
        _  -> do
            upgradePackages
            installPackages $ fromList missingPkgs

nullProvider :: PkgProvider
nullProvider = PkgProvider
    { return ()
    , \_ -> return False
    , \_ -> return ()
    }
