{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Development.Guardian.Types where

import Algebra.Graph (Graph)
import qualified Algebra.Graph as G
import Algebra.Graph.AdjacencyMap.Algorithm (Cycle)
import qualified Algebra.Graph.Class as GC
import Algebra.Graph.Label
import qualified Algebra.Graph.Labelled as L
import Algebra.Graph.Relation.Preorder (PreorderRelation)
import Control.Applicative
import Control.Exception (Exception)
import Control.Monad
import Control.Monad.Trans.Reader (ReaderT (ReaderT, runReaderT))
import Control.Monad.Trans.Writer.CPS (runWriter, tell)
import Data.Aeson (FromJSON (..), FromJSONKey, genericParseJSON, withObject, withText, (.:), (.:?))
import qualified Data.Aeson as J
import qualified Data.Bifunctor as Bi
import Data.Coerce (coerce)
import qualified Data.DList.DNonEmpty as DLNE
import Data.Foldable (fold)
import qualified Data.Foldable as F
import Data.Functor.Compose (Compose (..))
import Data.Generics.Labels ()
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, mapMaybe)
import Data.Monoid (Last (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import qualified Data.Trie as Trie
import qualified Data.Vector as V
import GHC.Generics (Generic)
import RIO ((^.))
import Text.Pattern (Pattern, concretePrefix)
import qualified Text.Pattern as P

type DomainGraph = PreorderRelation DomainName

type PackageGraph = Graph PackageName

type ActualGraph' e = L.Graph e DomainName

type ActualGraph = ActualGraph' (Path PackageName)

type PackageDic = Map PackageName (DomainName, V.Vector Dependency)

data DomainGraphError
  = CyclicDomainDep (Cycle DomainName)
  | OverlappingPackages (Map PackageName (Set DomainName))
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (Exception)

data CheckResult
  = Ok
  | OkWithDiagnostics Diagnostics
  deriving (Show, Eq, Ord, Generic)

isEmptyDiagnostics :: Diagnostics -> Bool
isEmptyDiagnostics Diagnostics {..} =
  Map.null redundantExtraDeps
    -- && Set.null redundantDomainDeps
    && Map.null usedExceptionalRules

data Diagnostics = Diagnostics
  { redundantExtraDeps :: Map PackageName (Set Dependency)
  , -- , redundantDomainDeps :: Set (DomainName, DomainName)
    usedExceptionalRules :: Map PackageName (Set Dependency)
  }
  deriving (Show, Eq, Ord, Generic)

data DomainInfo = DomainInfo
  { domainConfig :: Domains
  , domainGraph :: DomainGraph
  , packageDic :: PackageDic
  }
  deriving (Show, Eq, Ord, Generic)

data PackageViolation
  = DomainBoundaryViolation
      { fromDom :: DomainName
      , toDom :: DomainName
      , introducedBy :: [(PackageName, PackageName)]
      }
  | CyclicPackageDep (Cycle PackageName)
  | OrphanPackage PackageName [PackageName]
  | UncoveredPackages [PackageName]
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (Exception)

newtype PackageName = PackageName {getPackageName :: Text}
  deriving (Eq, Ord, Generic)
  deriving newtype (Show, IsString, FromJSON, Hashable)

newtype DomainName = DomainName {getDomainName :: Text}
  deriving (Eq, Ord, Generic)
  deriving newtype (Show, IsString, FromJSON, FromJSONKey, Hashable)

data Domain' a = Domain
  { dependsOn :: Maybe (V.Vector DomainName)
  , packages :: V.Vector (PackageDef' a)
  }
  deriving (Show, Eq, Ord, Generic, Functor, Foldable, Traversable)

type Domain = Domain' PackageName

instance FromJSON a => FromJSON (Domain' a) where
  parseJSON =
    genericParseJSON
      J.defaultOptions
        { J.omitNothingFields = True
        , J.fieldLabelModifier = J.camelTo2 '_'
        }

type PackageDef = PackageDef' PackageName

newtype Domains = Domains
  { getDomains :: HM.HashMap DomainName Domain
  }
  deriving (Show, Eq, Ord, Generic)

data DomainsConfig
  = -- | Domain definition without wildcards
    ConcreteDomains Domains
  | -- | Domain definition containing wildcards
    WildcardDomains (HM.HashMap DomainName (Domain' Pattern))
  deriving (Show, Eq, Ord, Generic)

instance FromJSON DomainsConfig where
  parseJSON = withObject "{domains: ... [, wildcards: true]}" $ \obj -> do
    wildcards <- obj .:? "wildcards" J..!= False
    if wildcards
      then WildcardDomains <$> obj .: "domains"
      else ConcreteDomains . Domains <$> obj .: "domains"

data DomainResult = DomainResult {domains :: Domains, warnings :: Maybe (NonEmpty String)}
  deriving (Show, Eq, Ord, Generic)

{- |
Resolves patterns in packages in the domains.
If the multiple patterns matched, it picks one with the longest prefix.
-}
resolveDomainName :: DomainsConfig -> PackageGraph -> DomainResult
resolveDomainName (ConcreteDomains doms) _ = DomainResult doms Nothing
resolveDomainName (WildcardDomains patDoms) pkgGraph =
  let pkgs = G.vertexList pkgGraph
      pats =
        Trie.fromList $
          Map.toList $
            Map.fromListWith (<>) $
              map ((,) <$> T.encodeUtf8 . concretePrefix <*> Set.singleton) $
                F.toList $
                  Compose patDoms
      pkgAssigns =
        fmap (V.fromList . Set.toList) $
          HM.fromList $
            Map.toList $
              Map.fromListWith (<>) $
                mapMaybe
                  ( \pn@(PackageName nm) ->
                      fmap (,Set.singleton pn)
                        $ getLast
                        $ foldMap
                          (\(_, ps, _) -> Last $ F.find (isJust . (`P.match` nm)) ps)
                        $ Trie.matches pats (T.encodeUtf8 nm)
                  )
                  pkgs
      (doms, warns) = Bi.second (fmap DLNE.toNonEmpty) $
        runWriter $
          forM patDoms $ \dom -> do
            pkgs' <- fmap fold $ V.forM (packages dom) $ \pkgDef -> do
              let aPat = pkgDef ^. #packageName
                  cands = HM.lookupDefault mempty aPat pkgAssigns
              when (V.null cands) $
                tell $
                  Just $
                    DLNE.singleton $
                      "No match found for pattern: " <> show aPat
              pure $ V.map (\pn -> pkgDef {packageName = pn}) cands
            pure dom {packages = pkgs'}
   in DomainResult {domains = Domains doms, warnings = warns}

data PackageDef' a = PackageDef
  { packageName :: !a
  , extraDeps :: V.Vector Dependency
  }
  deriving (Show, Eq, Ord, Generic, Functor, Foldable, Traversable)

instance FromJSON a => FromJSON (PackageDef' a) where
  parseJSON =
    runReaderT $
      ReaderT (fmap (`PackageDef` V.empty) . J.parseJSON)
        <|> ReaderT do
          withObject "object" $ \dic -> do
            packageName <- dic .: "package"
            excepts <- dic .:? "exception"
            extraDeps <- maybe (pure V.empty) (.: "depends_on") excepts
            pure PackageDef {..}

data Dependency
  = DomainDep !DomainName
  | PackageDep !PackageName
  deriving (Show, Eq, Ord, Generic)

instance FromJSON Dependency where
  parseJSON =
    runReaderT $
      ReaderT (withText "domain name" (pure . DomainDep . DomainName))
        <|> ReaderT
          ( withObject
              "{package: ...} or {domain: ...}"
              ( \obj ->
                  DomainDep
                    <$> obj
                    .: "domain"
                    <|> PackageDep
                    <$> obj
                    .: "package"
              )
          )

newtype Overlayed gr = Overlayed {getOverlayed :: gr}
  deriving (Show, Eq, Ord)

instance GC.Graph gr => Semigroup (Overlayed gr) where
  (<>) = coerce $ GC.overlay @gr

instance GC.Graph gr => Monoid (Overlayed gr) where
  mempty = Overlayed GC.empty
