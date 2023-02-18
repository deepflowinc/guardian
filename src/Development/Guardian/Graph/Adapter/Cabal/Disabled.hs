{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Development.Guardian.Graph.Adapter.Cabal.Disabled (
  buildPackageGraph,
  CustomPackageOptions (..),
  Cabal,
) where

import Data.Generics.Labels ()
import Development.Guardian.Graph.Adapter.Cabal.Types
import Development.Guardian.Graph.Adapter.Types (PackageGraphOptions (..))
import Development.Guardian.Types (PackageGraph)

buildPackageGraph :: PackageGraphOptions Cabal -> IO PackageGraph
buildPackageGraph _ = error "Cabal backend disabled!"
