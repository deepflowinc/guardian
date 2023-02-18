{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Development.Guardian.Graph.Adapter.Stack.Disabled (
  buildPackageGraphM,
  buildPackageGraph,
  Stack,
  CustomPackageOptions (..),
) where

import Development.Guardian.Graph.Adapter.Stack.Types
import Development.Guardian.Graph.Adapter.Types
import Development.Guardian.Types (PackageGraph)
import RIO (RIO)

buildPackageGraph :: PackageGraphOptions Stack -> IO PackageGraph
buildPackageGraph _ = error "buildPackageGraph: Stack adapter is disabled"

buildPackageGraphM :: RIO env PackageGraph
buildPackageGraphM = error "buildPackageGraphM: Stack adapter is disabled"
