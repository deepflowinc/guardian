{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Development.Guardian.Graph.Adapter.Cabal (
  buildPackageGraph,
  CustomPackageOptions (..),
  Cabal,
) where

import qualified Algebra.Graph as G
import Control.Applicative (optional, (<|>))
import Control.Monad (when)
import Data.Aeson (FromJSON, withObject, (.:))
import qualified Data.Aeson.KeyMap as AKM
import Data.Aeson.Types (FromJSON (..))
import Data.Either (fromLeft)
import Data.Generics.Labels ()
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Set as Set
import Data.String (fromString)
import Development.Guardian.Graph.Adapter.Types (ComponentsOptions (..), CustomPackageOptions, PackageGraphOptions (..))
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
import Distribution.Simple.Flag (Flag (..))
import Distribution.Verbosity (silent)
import GHC.Generics (Generic)
import Lens.Micro
import Path

data Cabal deriving (Generic)

data instance CustomPackageOptions Cabal = CabalOptions {projectFile :: Maybe (Path Rel File), update :: Maybe (Either Bool String)}
  deriving (Show, Eq, Ord, Generic)

instance FromJSON (CustomPackageOptions Cabal) where
  parseJSON = withObject "{cabal: ...}" $ \obj ->
    if AKM.member "cabal" obj
      then do
        dic <- obj .: "cabal"
        projectFile <- optional $ dic .: "projectFile"
        update <-
          if AKM.member "update" dic
            then
              fmap Just $
                Left <$> (dic .: "update")
                  <|> Right <$> (dic .: "update")
            else pure Nothing
        pure CabalOptions {..}
      else pure (CabalOptions Nothing Nothing)

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
