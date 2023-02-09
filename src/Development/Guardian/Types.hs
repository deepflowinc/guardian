{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Development.Guardian.Types where

import Algebra.Graph (Graph)
import Algebra.Graph.AdjacencyMap.Algorithm (Cycle)
import qualified Algebra.Graph.Class as GC
import Algebra.Graph.Label
import qualified Algebra.Graph.Labelled as L
import Algebra.Graph.Relation.Preorder (PreorderRelation)
import Control.Applicative
import Control.Exception (Exception)
import Control.Monad.Trans.Reader (ReaderT (ReaderT, runReaderT))
import Data.Aeson (FromJSON (..), FromJSONKey, genericParseJSON, withObject, withText, (.:), (.:?))
import qualified Data.Aeson as J
import Data.Coerce (coerce)
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Vector as V
import GHC.Generics (Generic)

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
  | UncoveredPackages [PackageName]
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (Exception)

newtype PackageName = PackageName {getPackageName :: Text}
  deriving (Eq, Ord, Generic)
  deriving newtype (Show, IsString, FromJSON, Hashable)

newtype DomainName = DomainName {getDomainName :: Text}
  deriving (Eq, Ord, Generic)
  deriving newtype (Show, IsString, FromJSON, FromJSONKey, Hashable)

data Domain = Domain
  { dependsOn :: Maybe (V.Vector DomainName)
  , packages :: V.Vector PackageDef
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON Domain where
  parseJSON =
    genericParseJSON
      J.defaultOptions
        { J.omitNothingFields = True
        , J.fieldLabelModifier = J.camelTo2 '_'
        }

newtype Domains = Domains {domains :: HM.HashMap DomainName Domain}
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON)

data PackageDef = PackageDef
  { packageName :: !PackageName
  , extraDeps :: V.Vector Dependency
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON PackageDef where
  parseJSON =
    runReaderT $
      ReaderT (withText "package name" (pure . (`PackageDef` V.empty) . PackageName))
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
                  DomainDep <$> obj .: "domain"
                    <|> PackageDep <$> obj .: "package"
              )
          )

newtype Overlayed gr = Overlayed {getOverlayed :: gr}
  deriving (Show, Eq, Ord)

instance GC.Graph gr => Semigroup (Overlayed gr) where
  (<>) = coerce $ GC.overlay @gr

instance GC.Graph gr => Monoid (Overlayed gr) where
  mempty = Overlayed GC.empty
