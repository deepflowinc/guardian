{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Development.Guardian.GraphSpec where

import qualified Algebra.Graph as G
import qualified Algebra.Graph.ToGraph as AM
import qualified Data.HashMap.Strict as HM
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Development.Guardian.Graph
import Development.Guardian.Types
import qualified GHC.OldList as OL
import Test.Tasty
import Test.Tasty.HUnit
import Validation

{- |
@
    A1--+      +--B1
    |   |      |
    |   v      v
    |  A2 --> B2
    v         |
    A3        +-->B3
    |             |
    |             |
    +---> C1 <----+
@
-}
packages1 :: PackageGraph
packages1 = G.edges [(a1, a3), (a1, a2), (a2, b2), (b1, b2), (b2, b3), (a3, c1), (b3, c1)]
  where
    (a1, a2, a3) = ("A1", "A2", "A3")
    (b1, b2, b3) = ("B1", "B2", "B3")
    c1 = "C1"

{- |
@     A           B
  +-------+  +-------+
  | A1    |<-+----B1 |
  |    A2-+--+->B2   |
  | A3    |  |    B3 |
  +---+---+  +---+---+
      |          |
      v     C    v
  +---+----------+----+
  |         C1        |
  |                   |
  +-------------------+
@
-}
domains1 :: Domains
domains1 =
  Domains $
    HM.fromList
      [ ("A", Domain {dependsOn = Just ["C"], packages = [a1, a2, a3]})
      , ("B", Domain {dependsOn = Just ["C"], packages = [b1, b2, b3]})
      , ("C", Domain {dependsOn = Nothing, packages = [c1]})
      ]
  where
    a1 = PackageDef {packageName = "A1", extraDeps = []}
    a2 = PackageDef {packageName = "A2", extraDeps = [PackageDep "B2"]}
    a3 = PackageDef {packageName = "A3", extraDeps = []}
    b1 = PackageDef {packageName = "B1", extraDeps = []}
    b2 = PackageDef {packageName = "B2", extraDeps = [DomainDep "A"]}
    b3 = PackageDef {packageName = "B3", extraDeps = []}
    c1 = PackageDef {packageName = "C1", extraDeps = []}

test_buildDomainInfo :: TestTree
test_buildDomainInfo =
  testGroup
    "buildDomainInfo"
    [ testCase "domains1 is valid" $ do
        let vinfo = buildDomainInfo domains1
        isSuccess vinfo @? "Domains #1 should be valid, but got: " <> show vinfo
    ]

test_detectPackageCycle :: TestTree
test_detectPackageCycle =
  testGroup
    "detectPackageCycle"
    [ testCase "validates packages1" $
        detectPackageCycle packages1 @?= Success ()
    , testCase "invalidates (packages1 + A2 -> A1)" $
        detectPackageCycle (packages1 `G.overlay` G.edge "A2" "A1")
          @?= Failure (CyclicPackageDep ("A2" :| ["A1"]))
    ]

test_validatePackageGraph :: TestTree
test_validatePackageGraph =
  testGroup
    "validatePackageGraph"
    [ testCase "packages1 is valid, with B2 -> (A) redundant" $ do
        doms <- fromDomainInfo $ buildDomainInfo domains1
        case validatePackageGraph doms packages1 of
          f@Failure {} -> assertFailure $ "Must success, but got: " <> show f
          Success (OkWithDiagnostics Diagnostics {usedExceptionalRules = useds, redundantExtraDeps = reds})
            | [("B2", [DomainDep "A"])] <- Map.toList reds
            , [("A2", [PackageDep "B2"])] <- useds ->
                pure ()
          s@Success {} -> assertFailure $ "B2 -> (A) must be reported redundant, but got: " <> show s
    , testCase "(packages1 + A2 -> B1) is invalid" $ do
        doms <- fromDomainInfo $ buildDomainInfo domains1
        let pg = packages1 `G.overlay` G.edge "A2" "B1"
        case validatePackageGraph doms pg of
          Failure
            ( DomainBoundaryViolation
                { fromDom = "A"
                , toDom = "B"
                , introducedBy = [("A2", "B1")]
                }
                :| []
              ) -> pure ()
          f ->
            assertFailure $
              "Must fail reporting A2 -> B1 as invalid, but got: "
                <> show f
    , testCase ("detects package cycle in " <> show (AM.toAdjacencyMap $ packages1 `G.overlay` G.edge "A2" "A1")) $ do
        doms <- fromDomainInfo $ buildDomainInfo domains1
        let pg = packages1 `G.overlay` G.edge "A2" "A1"
        case validatePackageGraph doms pg of
          Failure (CyclicPackageDep cyc :| [])
            | cyc `OL.elem` ["A2" :| ["A1"], "A1" :| ["A2"]] -> pure ()
          f ->
            assertFailure $
              "Must fail reporting A2 -> A1 -> A2 as valid, but got: "
                <> show f
    ]

fromDomainInfo :: Validation (NonEmpty DomainGraphError) DomainInfo -> IO DomainInfo
fromDomainInfo =
  validation
    (assertFailure . ("Invalid Domain: " <>) . show . NE.toList)
    pure
