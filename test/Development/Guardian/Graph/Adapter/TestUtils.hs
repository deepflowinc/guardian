{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Development.Guardian.Graph.Adapter.TestUtils (
  Case (..),
  testProjectWith,
  testProjectWithLabel,
) where

import Development.Guardian.Graph.Adapter.Types
import Development.Guardian.Types
import Path
import Path.IO
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, (@?=))

data Case adapter = Case
  { caseDir :: Path Rel Dir
  , expectedGraph :: PackageGraph
  , componentOpts :: ComponentsOptions
  , customOpts :: CustomPackageOptions adapter
  }

deriving instance Eq (CustomPackageOptions adapter) => Eq (Case adapter)

deriving instance Ord (CustomPackageOptions adapter) => Ord (Case adapter)

deriving instance Show (CustomPackageOptions adapter) => Show (Case adapter)

testProjectWith ::
  (PackageGraphOptions adapter -> IO PackageGraph) ->
  Case adapter ->
  TestTree
testProjectWith build = testProjectWithLabel build <$> fromRelDir . caseDir <*> id

testProjectWithLabel ::
  (PackageGraphOptions adapter -> IO PackageGraph) ->
  String ->
  Case adapter ->
  TestTree
testProjectWithLabel buildGraph label Case {..} = testCase name $ do
  targetPath <- canonicalizePath $ [reldir|data|] </> caseDir

  let components = componentOpts
      customOptions = customOpts
  graph <- buildGraph PackageGraphOptions {..}
  graph @?= expectedGraph
  where
    ComponentsOptions {..} = componentOpts
    sgn True = "+"
    sgn False = "-"
    name =
      label
        <> " ("
        <> sgn tests
        <> "test, "
        <> sgn benchmarks
        <> "bench"
        <> ")"
