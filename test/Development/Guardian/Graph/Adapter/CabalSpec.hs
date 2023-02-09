{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Development.Guardian.Graph.Adapter.CabalSpec (test_buildPackageGraph) where

import qualified Algebra.Graph as G
import Development.Guardian.Graph.Adapter.Cabal
import qualified Development.Guardian.Graph.Adapter.Cabal as Cabal
import Development.Guardian.Graph.Adapter.TestUtils
import Development.Guardian.Graph.Adapter.Types
import Path
import Test.Tasty

onlyExes :: ComponentsOptions
onlyExes = ComponentsOptions {tests = False, benchmarks = False}

test_buildPackageGraph :: TestTree
test_buildPackageGraph =
  testGroup
    "buildPackageGraph"
    $ map
      testCabalProject
      [ Case
          { caseDir = [reldir|test-only-dependency|]
          , expectedGraph = G.edges [("A1", "C"), ("A2", "C"), ("B1", "C")]
          , componentOpts = onlyExes
          , customOpts = CabalOptions Nothing Nothing
          }
      , Case
          { caseDir = [reldir|test-only-dependency|]
          , expectedGraph = G.edges [("A1", "C"), ("A2", "C"), ("A2", "B1"), ("B1", "C")]
          , componentOpts = onlyExes {tests = True}
          , customOpts = CabalOptions Nothing Nothing
          }
      , Case
          { caseDir = [reldir|test-only-dependency|]
          , expectedGraph = G.edges [("A1", "C"), ("A1", "A2"), ("A2", "C"), ("B1", "C")]
          , componentOpts = onlyExes {benchmarks = True}
          , customOpts = CabalOptions Nothing Nothing
          }
      , Case
          { caseDir = [reldir|test-only-dependency|]
          , expectedGraph = G.edges [("A1", "C"), ("A1", "A2"), ("A2", "C"), ("A2", "B1"), ("B1", "C")]
          , componentOpts = onlyExes {tests = True, benchmarks = True}
          , customOpts = CabalOptions Nothing Nothing
          }
      ]
      ++ [ testCabalProjectWithLabel
            "test-only-dependency (with cabal-no-A1.yaml)"
            Case
              { caseDir = [reldir|test-only-dependency|]
              , expectedGraph = G.edges [("A2", "C"), ("B1", "C")]
              , componentOpts = onlyExes
              , customOpts = CabalOptions (Just [relfile|cabal-no-A1.project|]) Nothing
              }
         ]

testCabalProjectWithLabel :: String -> Case Cabal -> TestTree
testCabalProjectWithLabel = testProjectWithLabel Cabal.buildPackageGraph

testCabalProject :: Case Cabal -> TestTree
testCabalProject = testProjectWith Cabal.buildPackageGraph
