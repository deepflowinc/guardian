{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Development.Guardian.Graph.Adapter.Custom (
  Custom,
  buildPackageGraph,
  CustomPackageOptions (..),
  CustomAdapterException (..),
  fromDotGraph,
) where

import qualified Algebra.Graph as G
import Control.Applicative (liftA2, (<|>))
import Control.Exception (Exception, throwIO)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON)
import qualified Data.Aeson as J
import qualified Data.Bifunctor as Bi
import qualified Data.CaseInsensitive as CI
import qualified Data.DList.DNonEmpty as DLNE
import Data.Function (on, (&))
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Monoid (Ap (..))
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import Development.Guardian.Graph.Adapter.Types
import Development.Guardian.Types
import GHC.Generics (Generic)
import Language.Dot (parseDot)
import qualified Language.Dot as Dot
import Path (File, SomeBase, fromAbsDir, fromAbsFile)
import Path.IO (makeAbsolute)
import RIO (tshow)
import System.Environment (getEnvironment)
import System.Process.Typed (ProcessConfig, proc, readProcessStdout_, setEnv, shell)
import qualified Text.Parsec as Parsec
import Validation

data Custom

newtype instance CustomPackageOptions Custom = CustomOptions {custom :: CustomOpts}
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (J.FromJSON)

data CustomOpts = CustomOpts
  { adapter :: ExternalProcess
  , ignoreLoop :: Bool
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON CustomOpts where
  parseJSON = J.withObject "dict" $ \obj -> do
    adapter <-
      Shell <$> (obj J..: "shell")
        <|> Program <$> (obj J..: "program")
    ignoreLoop <- obj J..:? "ignore_loop" J..!= False
    pure CustomOpts {..}

data ExternalProcess = Shell Text | Program (SomeBase File)
  deriving (Show, Eq, Ord, Generic)

createExternalAdapter ::
  MonadIO m =>
  PackageGraphOptions Custom ->
  m (ProcessConfig () () ())
createExternalAdapter PackageGraphOptions {..} = do
  env0 <- liftIO getEnvironment
  cfg0 <- case adapter $ custom customOptions of
    Shell txt ->
      pure $ shell $ fromString $ T.unpack txt
    Program prog -> do
      progAbs <- makeAbsolute prog
      pure $ proc (fromAbsFile progAbs) [fromAbsDir targetPath]
  let componentEnvs =
        [(includeTestVar, "1") | tests components]
          ++ [(includeBenchVar, "1") | benchmarks components]
      env' =
        ("GUARDIAN_ROOT_DIR", fromAbsDir targetPath)
          : componentEnvs
          ++ filter ((`notElem` [includeTestVar, includeBenchVar]) . fst) env0
  pure $ cfg0 & setEnv env'

includeBenchVar :: String
includeBenchVar = "GUARDIAN_INCLUDE_BENCHMARKS"

includeTestVar :: String
includeTestVar = "GUARDIAN_INCLUDE_TESTS"

data CustomAdapterException
  = InvalidAdapterOutput String Parsec.ParseError
  | InvalidGraph String Dot.Graph (NonEmpty GraphViolation)
  deriving (Eq, Generic)
  deriving anyclass (Exception)

instance Show CustomAdapterException where
  showsPrec d exc = showParen (d > 10) $
    case exc of
      InvalidAdapterOutput src pe ->
        showString "Parse error in adapter output:\n"
          . showString "Error: \n"
          . showString (unlines $ map ('\t' :) $ lines $ show pe)
          . showString "Input: \n"
          . showString (unlines $ map ('\t' :) $ lines src)
      InvalidGraph src gr ne ->
        showString "Invalid Dot Graph was passed: "
          . shows (NE.toList ne)
          . showString "\nParsedGraph:\n\t"
          . shows gr
          . showString "\nInput:\n"
          . showString (unlines $ map ('\t' :) $ lines src)

data GraphViolation
  = DirectedGraphExpected
  | EdgeToSubgraphNotSupported (Maybe Dot.Id)
  | MultiEdgeNotSupported [String]
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (Exception)

buildPackageGraph :: PackageGraphOptions Custom -> IO PackageGraph
buildPackageGraph opts = do
  pc <- createExternalAdapter opts
  src <- LT.unpack . LT.decodeUtf8 <$> readProcessStdout_ pc
  gr <- either (throwIO . InvalidAdapterOutput src) pure $ parseDot "<adapter output>" src
  let pkgGr = fromDotGraph (custom $ customOptions opts) gr
  either (throwIO . InvalidGraph src gr) pure pkgGr

data DotEntity = DotId Text | Pkg PackageName
  deriving (Show, Eq, Ord, Generic)

fromDotGraph :: CustomOpts -> Dot.Graph -> Either (NonEmpty GraphViolation) PackageGraph
fromDotGraph CustomOpts {..} (Dot.Graph _ dir _ stmts) =
  Bi.first DLNE.toNonEmpty $
    validationToEither $
      when
        (dir /= Dot.DirectedGraph)
        (failed DirectedGraphExpected)
        *> getAp (foldMap (Ap . (liftA2 (,) <$> go <*> (pure . buildDic))) stmts)
        <&> \(gr0, dic) ->
          fmap
            ( \case
                DotId txt -> fromMaybe (PackageName txt) $ Map.lookup txt dic
                Pkg pn -> pn
            )
            gr0
  where
    buildDic (Dot.NodeStatement ni atts) =
      Map.singleton (prettyNodeId ni) $ parseNode ni atts
    buildDic _ = mempty
    go (Dot.NodeStatement ni atts) =
      pure $
        G.vertex $
          Pkg $
            parseNode ni atts
    go (Dot.EdgeStatement [l, r] _)
      | ignoreLoop, parseEntity l == parseEntity r = mempty
      | otherwise =
          (G.connect `on` G.vertex) <$> parseEntity l <*> parseEntity r
    go (Dot.EdgeStatement ents _) =
      failed $ MultiEdgeNotSupported $ map (show . Dot.pp) ents
    go Dot.AttributeStatement {} = pure mempty
    go Dot.AssignmentStatement {} = pure mempty
    go Dot.SubgraphStatement {} = mempty

prettyNodeId :: Dot.NodeId -> Text
prettyNodeId (Dot.NodeId ni _) = prettyId ni

parseNode :: Dot.NodeId -> [Dot.Attribute] -> PackageName
parseNode (Dot.NodeId origId _) attrs =
  PackageName $
    prettyId $
      fromMaybe origId $
        lookup
          (CI.mk "label")
          [ (CI.mk $ prettyId l, v)
          | Dot.AttributeSetValue l v <- attrs
          ]

prettyId :: Dot.Id -> Text
prettyId (Dot.NameId s) = T.pack s
prettyId (Dot.StringId s) = T.pack s
prettyId (Dot.IntegerId n) = tshow n
prettyId (Dot.FloatId x) = tshow x
prettyId (Dot.XmlId xml) = tshow $ Dot.pp xml

failed :: e -> Validation (DLNE.DNonEmpty e) b
failed = Failure . DLNE.singleton

parseEntity :: Dot.Entity -> Validation (DLNE.DNonEmpty GraphViolation) DotEntity
parseEntity (Dot.ENodeId _ (Dot.NodeId ni _)) = pure $ DotId $ prettyId ni
parseEntity (Dot.ESubgraph _ sub) = failed $ EdgeToSubgraphNotSupported $ subGraphId sub

subGraphId :: Dot.Subgraph -> Maybe Dot.Id
subGraphId (Dot.NewSubgraph mid _) = mid
subGraphId (Dot.SubgraphRef ident) = Just ident
