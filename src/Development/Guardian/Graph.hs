{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Development.Guardian.Graph where

import qualified Algebra.Graph as G
import qualified Algebra.Graph.Class as GC
import Algebra.Graph.Label (Path)
import qualified Algebra.Graph.Labelled as LG
import qualified Algebra.Graph.Relation as Rel
import qualified Algebra.Graph.Relation.Preorder as Preorder
import qualified Algebra.Graph.ToGraph as GC
import Control.Monad (guard, void)
import Data.Bifunctor (Bifunctor)
import qualified Data.Bifunctor as Bi
import Data.Coerce (coerce)
import qualified Data.DList as DL
import qualified Data.DList.DNonEmpty as DLNE
import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Semigroup.Generic
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector as V
import Development.Guardian.Types (
  ActualGraph,
  CheckResult (..),
  Dependency (..),
  Diagnostics (Diagnostics, redundantExtraDeps, usedExceptionalRules),
  Domain (Domain, dependsOn, packages),
  DomainGraph,
  DomainGraphError (..),
  DomainInfo (..),
  DomainName,
  Domains (..),
  Overlayed (Overlayed, getOverlayed),
  PackageDef (PackageDef, extraDeps, packageName),
  PackageDic,
  PackageGraph,
  PackageName,
  PackageViolation (
    CyclicPackageDep,
    DomainBoundaryViolation,
    UncoveredPackages
  ),
  isEmptyDiagnostics,
 )
import GHC.Generics (Generic)
import Validation

buildDomainInfo :: Domains -> Validation (NE.NonEmpty DomainGraphError) DomainInfo
buildDomainInfo domainConfig = do
  let packageDic = buildPackageDic domainConfig
  domainGraph <- toDomainGraph domainConfig
  pure DomainInfo {..}

buildRawDomainGraph ::
  (GC.Graph gr, GC.Vertex gr ~ DomainName) =>
  Domains ->
  gr
buildRawDomainGraph Domains {..} =
  getOverlayed $
    HM.foldMapWithKey
      ( \dom Domain {..} ->
          Overlayed (GC.vertex dom)
            <> maybe mempty (foldMap (Overlayed . GC.edge dom)) dependsOn
      )
      domains

toDomainGraph :: Domains -> Validation (NE.NonEmpty DomainGraphError) DomainGraph
toDomainGraph doms =
  Bi.first DLNE.toNonEmpty $
    ans
      <$ Bi.first DLNE.singleton (detectCycle raw)
      <* Bi.first DLNE.singleton (detectPackageOverlaps doms)
  where
    !raw = buildRawDomainGraph doms
    !ans = Preorder.fromRelation raw

detectPackageOverlaps ::
  Domains -> Validation DomainGraphError ()
detectPackageOverlaps Domains {..}
  | Map.null overlaps = Success ()
  | otherwise = Failure $ OverlappingPackages overlaps
  where
    overlaps =
      Map.filter ((> 1) . Set.size) $
        getCatMap $
          HM.foldMapWithKey
            ( \dom ->
                foldMap
                  ( CatMap
                      . flip Map.singleton (Set.singleton dom)
                      . packageName
                  )
                  . packages
            )
            domains

newtype CatMap k v = CatMap {getCatMap :: Map k v}

instance (Semigroup v, Ord k) => Semigroup (CatMap k v) where
  (<>) = coerce $ Map.unionWith @k @v (<>)
  {-# INLINE (<>) #-}

instance (Semigroup v, Ord k) => Monoid (CatMap k v) where
  mempty = CatMap Map.empty
  {-# INLINE mempty #-}

detectCycle ::
  (GC.ToGraph t, Ord (GC.ToVertex t), GC.ToVertex t ~ DomainName) =>
  t ->
  Validation DomainGraphError ()
detectCycle gr =
  Bi.first CyclicDomainDep $
    eitherToValidation $
      void $
        GC.topSort gr

buildPackageDic :: Domains -> PackageDic
buildPackageDic =
  HM.foldMapWithKey
    ( \domName Domain {..} ->
        Map.fromList $
          map (\PackageDef {..} -> (packageName, (domName, extraDeps))) $
            V.toList packages
    )
    . domains

matches :: PackageDic -> Dependency -> PackageName -> Bool
matches pkgDic (DomainDep dn) pkg =
  maybe False ((dn ==) . fst) $ Map.lookup pkg pkgDic
matches _ (PackageDep pn) pkg = pn == pkg

newtype LOverlayed e a = LOverlayed {getLOverlayed :: LG.Graph e a}
  deriving (Show, Eq, Ord, Generic)

instance Monoid e => Semigroup (LOverlayed e a) where
  (<>) = coerce $ LG.overlay @e @a
  {-# INLINE (<>) #-}

instance Monoid e => Monoid (LOverlayed e a) where
  mempty = LOverlayed LG.empty
  {-# INLINE mempty #-}

data ActualGraphs a b = AGs {activatedGraph :: a, exceptionGraph :: b}
  deriving (Functor, Show, Eq, Ord, Generic)
  deriving (Semigroup, Monoid) via GenericSemigroupMonoid (ActualGraphs a b)

instance Bifunctor ActualGraphs where
  bimap f g (AGs x y) = AGs (f x) (g y)
  {-# INLINE bimap #-}
  first f (AGs x y) = AGs (f x) y
  {-# INLINE first #-}
  second g (AGs x y) = AGs x (g y)
  {-# INLINE second #-}

buildActualGraphs ::
  PackageDic ->
  PackageGraph ->
  ActualGraphs ActualGraph (Map PackageName (Set Dependency))
buildActualGraphs pkgDic =
  Bi.bimap
    (Bi.first DL.toList . getLOverlayed)
    (Map.filter (not . Set.null) . getCatMap)
    . foldMap
      ( \e@(src, dst) ->
          let (srcDomain, srcExcept) =
                fromMaybe (error $ "src, not found: " <> show (src, pkgDic)) $
                  Map.lookup src pkgDic
              (dstDomain, _) =
                fromMaybe (error $ "dst, not found: " <> show (dst, pkgDic)) $
                  Map.lookup dst pkgDic
              aGraph = LOverlayed $ LG.edge (DL.singleton e) srcDomain dstDomain
              excepts = Set.fromList $ V.toList $ V.filter (flip (matches pkgDic) dst) srcExcept
           in if Set.null excepts
                then AGs {exceptionGraph = mempty, activatedGraph = aGraph}
                else AGs {activatedGraph = mempty, exceptionGraph = CatMap $ Map.singleton src excepts}
      )
    . G.edgeList

validatePackageGraph ::
  DomainInfo -> PackageGraph -> Validation (NE.NonEmpty PackageViolation) CheckResult
validatePackageGraph DomainInfo {..} pg =
  Bi.first DLNE.toNonEmpty $
    resl
      <$ ( case Bi.first DLNE.singleton (detectPackageCycle pg) of
            f@Failure {} -> f
            Success {} ->
              Bi.first DLNE.singleton (coversAllPackages packageDic pg)
                <* satisfiesDomainGraph domainGraph activatedGraph
         )
  where
    AGs {..} = buildActualGraphs packageDic pg
    redundantExtras = findRedundantExtraDeps packageDic pg
    diags =
      Diagnostics
        { redundantExtraDeps =
            Set.fromList . V.toList <$> redundantExtras
        , usedExceptionalRules = exceptionGraph
        }
    resl
      | isEmptyDiagnostics diags = Ok
      | otherwise = OkWithDiagnostics diags

detectPackageCycle ::
  PackageGraph -> Validation PackageViolation ()
detectPackageCycle pkgs =
  void $
    eitherToValidation $
      Bi.first CyclicPackageDep $
        GC.topSort pkgs

type ExemptDomDeps = ActualGraph

findRedundantExtraDeps ::
  PackageDic ->
  PackageGraph ->
  Map PackageName (V.Vector Dependency)
findRedundantExtraDeps pkgDic pg =
  Map.mapMaybeWithKey
    ( \pkg (_, specifiedDeps) -> do
        let actualDepPkgs = GC.postSet pkg pg
            actualDepDoms =
              Set.map
                (\dpkg -> maybe (error $ "No pkg find: " <> show (dpkg, actualDepPkgs)) fst (Map.lookup dpkg pkgDic))
                actualDepPkgs
            deps =
              V.filter
                ( \case
                    DomainDep dn -> dn `Set.notMember` actualDepDoms
                    PackageDep pn -> pn `Set.notMember` actualDepPkgs
                )
                specifiedDeps
        guard $ not $ V.null deps
        pure deps
    )
    pkgDic

coversAllPackages ::
  PackageDic -> PackageGraph -> Validation PackageViolation ()
coversAllPackages pkgDic pg =
  if null remain
    then Success ()
    else Failure $ UncoveredPackages remain
  where
    remain = Set.toList $ G.vertexSet pg Set.\\ Map.keysSet pkgDic

satisfiesDomainGraph ::
  DomainGraph -> ActualGraph -> Validation (DLNE.DNonEmpty PackageViolation) ()
satisfiesDomainGraph domGr ag =
  maybeToFailure () (DLNE.fromNonEmpty <$> NE.nonEmpty violatingEdges)
  where
    expectedEdges = Rel.edgeSet $ Preorder.toRelation domGr
    actualEdges :: Map (DomainName, DomainName) (Path PackageName)
    actualEdges =
      Map.fromList $
        map (\(x1, dn, dn') -> ((dn, dn'), x1)) $
          LG.edgeList ag
    violatingEdges =
      map
        ( uncurry $ uncurry DomainBoundaryViolation
        )
        $ Map.toList
        $ actualEdges `Map.withoutKeys` expectedEdges
