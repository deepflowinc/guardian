{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Development.Guardian.Graph.Adapter.Cabal.Enabled (
  buildPackageGraph,
  CustomPackageOptions (..),
  Cabal,
) where

import qualified Algebra.Graph as G
import Control.Monad (when)
import Data.Either (fromLeft)
import Data.Function ((&))
import Data.Generics.Labels ()
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Set as Set
import Data.String (fromString)
import Development.Guardian.Graph.Adapter.Cabal.Types
import Development.Guardian.Graph.Adapter.Types (ComponentsOptions (..), PackageGraphOptions (..))
import Development.Guardian.Types (Overlayed (..), PackageGraph)
import qualified Development.Guardian.Types as Guard
import Distribution.Client.CmdUpdate (updateAction)
import Distribution.Client.InstallPlan (GenericPlanPackage (..), depends)
import qualified Distribution.Client.InstallPlan as Plan
import Distribution.Client.NixStyleOptions (defaultNixStyleFlags)
import Distribution.Client.ProjectConfig (ProjectRoot (..))
import Distribution.Client.ProjectOrchestration (CurrentCommand (..), ProjectBaseContext (..), establishProjectBaseContextWithRoot, withInstallPlan)
import Distribution.Client.ProjectPlanning (ElaboratedConfiguredPackage (..), elabLocalToProject)
import Distribution.Package (Package (..), packageName, unPackageName)
import Distribution.Simple.Flag (pattern Flag)
import Distribution.Verbosity (silent)
import Path
import RIO ((.~))

buildPackageGraph :: PackageGraphOptions Cabal -> IO PackageGraph
buildPackageGraph PackageGraphOptions {customOptions = CabalOptions {..}, ..} = do
  let target = fromAbsDir targetPath
      mproj = fromAbsFile . (targetPath </>) <$> projectFile
      root = maybe (ProjectRootImplicit target) (ProjectRootExplicit target) mproj
  when (maybe False (fromLeft True) update) $ do
    let targets
          | Just (Right idx) <- update = [idx]
          | otherwise = []
    updateAction (defaultNixStyleFlags ()) targets mempty

  ctx0 <- establishProjectBaseContextWithRoot silent mempty root OtherCommand
  let pjCfg' =
        projectConfig ctx0
          & #projectConfigLocalPackages . #packageConfigTests
            .~ Flag (tests components)
          & #projectConfigLocalPackages . #packageConfigBenchmarks
            .~ Flag (benchmarks components)
      -- ProjectBaseContext has no Generic instance...
      ctx = ctx0 {projectConfig = pjCfg'}

  withInstallPlan silent ctx $ \iplan _scfg -> do
    let localPkgDic =
          Map.mapMaybe
            ( \case
                Configured pkg
                  | elabLocalToProject pkg -> pure pkg
                Installed pkg
                  | elabLocalToProject pkg -> pure pkg
                _ -> Nothing
            )
            $ Plan.toMap iplan
        localUnitIds = Map.keysSet localPkgDic
        gr =
          getOverlayed $
            foldMap
              ( \pkg ->
                  let srcPkg = packageName' pkg
                      deps =
                        filter (/= srcPkg)
                          $ mapMaybe
                            (fmap packageName' . (`Map.lookup` localPkgDic))
                          $ filter (`Set.member` localUnitIds)
                          $ depends pkg
                   in foldMap (Overlayed . G.edge srcPkg) deps
              )
              localPkgDic
    pure gr

packageName' :: Package pkg => pkg -> Guard.PackageName
packageName' = fromString . unPackageName . packageName
