{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Development.Guardian.Graph.Adapter.StackSpec where

import qualified Algebra.Graph as G
import Development.Guardian.Graph.Adapter.Stack
import qualified Development.Guardian.Graph.Adapter.Stack as Stack
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
      testStackProject
      [ Case
          { caseDir = [reldir|test-only-dependency|]
          , expectedGraph = G.edges [("A1", "C"), ("A2", "C"), ("B1", "C")]
          , componentOpts = onlyExes
          , customOpts = StackOptions ["--silent"]
          }
      , Case
          { caseDir = [reldir|test-only-dependency|]
          , expectedGraph = G.edges [("A1", "C"), ("A2", "C"), ("A2", "B1"), ("B1", "C")]
          , componentOpts = onlyExes {tests = True}
          , customOpts = StackOptions ["--silent"]
          }
      , Case
          { caseDir = [reldir|test-only-dependency|]
          , expectedGraph = G.edges [("A1", "C"), ("A1", "A2"), ("A2", "C"), ("B1", "C")]
          , componentOpts = onlyExes {benchmarks = True}
          , customOpts = StackOptions ["--silent"]
          }
      , Case
          { caseDir = [reldir|test-only-dependency|]
          , expectedGraph = G.edges [("A1", "C"), ("A1", "A2"), ("A2", "C"), ("A2", "B1"), ("B1", "C")]
          , componentOpts = onlyExes {tests = True, benchmarks = True}
          , customOpts = StackOptions ["--silent"]
          }
      ]
      ++ [ testStackProjectWithLabel
            "test-only-dependency (with stack-no-A1.yaml)"
            Case
              { caseDir = [reldir|test-only-dependency|]
              , expectedGraph = G.edges [("A2", "C"), ("B1", "C")]
              , componentOpts = onlyExes
              , customOpts = StackOptions ["--stack-yaml=stack-no-A1.yaml", "--silent"]
              }
         ]

testStackProjectWithLabel :: String -> Case Stack -> TestTree
testStackProjectWithLabel = testProjectWithLabel Stack.buildPackageGraph

testStackProject :: Case Stack -> TestTree
testStackProject = testProjectWith Stack.buildPackageGraph
