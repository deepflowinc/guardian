{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Development.Guardian.Graph.Adapter.Types (
  PackageBuildParser (..),
  CustomPackageOptions,
  PackageGraphOptions (..),
  ComponentsOptions (..),
  StandardAdapters (..),
) where

import Data.Aeson (FromJSON (..))
import qualified Data.Aeson as J
import GHC.Generics (Generic)
import Path (Abs, Dir, Path)

data StandardAdapters = Stack | Cabal | Custom
  deriving (Show, Eq, Ord)

data family CustomPackageOptions backend

newtype PackageBuildParser backend = PackageBuildParser
  { withTargetPath ::
      Path Abs Dir ->
      PackageGraphOptions backend
  }

instance
  FromJSON (CustomPackageOptions backend) =>
  FromJSON (PackageBuildParser backend)
  where
  parseJSON obj = do
    customOptions <- parseJSON obj
    components <- parseJSON obj
    pure $ PackageBuildParser $ \targetPath -> PackageGraphOptions {..}

data PackageGraphOptions backend = PackageGraphOptions
  { targetPath :: !(Path Abs Dir)
  , components :: !ComponentsOptions
  , customOptions :: CustomPackageOptions backend
  }
  deriving (Generic)

deriving instance
  Show (CustomPackageOptions backend) =>
  Show (PackageGraphOptions backend)

deriving instance
  Eq (CustomPackageOptions backend) =>
  Eq (PackageGraphOptions backend)

deriving instance
  Ord (CustomPackageOptions backend) =>
  Ord (PackageGraphOptions backend)

data ComponentsOptions = ComponentsOptions
  { tests :: Bool
  , benchmarks :: Bool
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON ComponentsOptions where
  parseJSON = J.withObject "{components: ...}" $ \dic -> do
    mcomp <- dic J..:? "components"
    case mcomp of
      Nothing -> pure ComponentsOptions {tests = True, benchmarks = True}
      Just comps -> do
        tests <- comps J..: "tests" J..!= True
        benchmarks <- comps J..: "benchmarks" J..!= True
        pure ComponentsOptions {..}
