{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Development.Guardian.App (
  BuildInfo (..),
  defaultMain,
  defaultMainWith,
  reportPackageGraphValidation,
  buildInfoQ,
) where

import Control.Applicative ((<**>))
import qualified Data.Aeson as J
import Data.Foldable.WithIndex
import Data.Functor.WithIndex.Instances ()
import Data.List (intersperse)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Monoid (Sum (..))
import qualified Data.Set as Set
import Data.Version (Version (..), showVersion)
import qualified Data.Yaml as Y
import Development.Guardian.Constants (configFileName)
import Development.Guardian.Graph
import qualified Development.Guardian.Graph.Adapter.Cabal as Cabal
import Development.Guardian.Graph.Adapter.Detection (detectAdapterThrow)
import qualified Development.Guardian.Graph.Adapter.Stack as Stack
import Development.Guardian.Graph.Adapter.Types
import Development.Guardian.Types
import GitHash (GitInfo, giDirty, giHash, tGitInfoCwdTry)
import Language.Haskell.TH.Syntax
import qualified Options.Applicative as Opts
import Path
import Path.IO (canonicalizePath, getCurrentDir)
import RIO
import System.Environment (getArgs)
import Validation (validation, validationToEither)

data Option = Option
  { mode :: Maybe StandardAdapters
  , target :: Maybe (SomeBase Dir)
  , config :: Path Rel File
  }
  deriving (Show, Eq, Ord)

data BuildInfo = BuildInfo {versionString :: String, gitInfo :: Maybe GitInfo}
  deriving (Show)

buildInfoQ :: Version -> Code Q BuildInfo
buildInfoQ version =
  [||
  BuildInfo
    { versionString = $$(liftTyped $ showVersion version)
    , gitInfo = either (const Nothing) Just $$(tGitInfoCwdTry)
    }
  ||]

optsPI :: BuildInfo -> Opts.ParserInfo Option
optsPI BuildInfo {..} = Opts.info (p <**> Opts.helper <**> versions) mempty
  where
    gitRev =
      maybe
        ""
        ( \gi ->
            ", Git revision "
              <> giHash gi
              <> if giDirty gi then " (dirty)" else ""
        )
        gitInfo
    verStr = "Guardian Version " <> versionString <> gitRev
    versions = Opts.infoOption verStr (Opts.long "version" <> Opts.short 'V' <> Opts.help "Prints version string and exit.")
    inP mode = do
      config <-
        Opts.option (Opts.eitherReader parsFileP) $
          Opts.long "config"
            <> Opts.short 'c'
            <> Opts.metavar "PATH"
            <> Opts.value configFileName
            <> Opts.showDefault
            <> Opts.help "configuration file, relative to the target directory"

      target <-
        optional $
          Opts.argument (Opts.eitherReader parseDirP) $
            Opts.help "input directory (uses current directory if missing)" <> Opts.metavar "DIR"
      pure Option {..}
    autoP =
      Opts.info (inP Nothing) $
        Opts.progDesc "Defends borders against the auto-detected build-system"

    p =
      Opts.hsubparser
        ( mconcat
            [ Opts.command "auto" autoP
            , Opts.command
                "stack"
                ( Opts.info (inP $ Just Stack) $
                    Opts.progDesc "Defends borders against stack.yaml"
                )
            , Opts.command
                "cabal"
                ( Opts.info (inP $ Just Cabal) $
                    Opts.progDesc "Defends borders against cabal.project"
                )
            ]
        )

parseDirP :: String -> Either String (SomeBase Dir)
parseDirP = first show . parseSomeDir

parsFileP :: String -> Either String (Path Rel File)
parsFileP = first show . parseRelFile

data GuardianException
  = InvalidDependencyDomainYaml [DomainGraphError]
  | PackageGraphErrors [PackageViolation]
  deriving (Show, Eq, Ord)
  deriving anyclass (Exception)

eitherResult :: J.Result a -> Either String a
eitherResult (J.Error s) = Left s
eitherResult (J.Success a) = Right a

defaultMainWith ::
  (MonadUnliftIO m, MonadReader env m, HasLogFunc env) =>
  BuildInfo ->
  [String] ->
  m ()
defaultMainWith buildInfo args = do
  Option {..} <-
    liftIO $
      Opts.handleParseResult $
        Opts.execParserPure Opts.defaultPrefs (optsPI buildInfo) args
  targ <- maybe getCurrentDir canonicalizePath target
  logInfo $ "Using configuration: " <> fromString (fromRelFile config)
  yaml <- Y.decodeFileThrow (fromAbsFile $ targ </> config)
  doms <- either throwString pure $ eitherResult $ J.fromJSON yaml

  mode' <- maybe (detectAdapterThrow yaml targ) pure mode
  logInfo $
    "Checking dependency of "
      <> displayShow targ
      <> " with backend "
      <> displayShow mode'
  pkgGraph <- case mode' of
    Stack ->
      liftIO $
        either throwString (Stack.buildPackageGraph . flip withTargetPath targ) $
          eitherResult $
            J.fromJSON yaml
    Cabal ->
      liftIO $
        either throwString (Cabal.buildPackageGraph . flip withTargetPath targ) $
          eitherResult $
            J.fromJSON yaml
  reportPackageGraphValidation doms pkgGraph

reportPackageGraphValidation ::
  (MonadReader env m, MonadIO m, HasLogFunc env) =>
  Domains ->
  PackageGraph ->
  m ()
reportPackageGraphValidation doms pkgGraph = do
  let mdomInfo = buildDomainInfo doms
  domInfo <-
    validation
      ( \errs -> do
          logError "Errors exists in dependency domain definition!"
          mapM_ (logError . displayShow) errs
          exitFailure
      )
      pure
      mdomInfo
  let chkResult =
        validationToEither $
          validatePackageGraph domInfo pkgGraph
  case chkResult of
    Left errs -> do
      logError $
        "Dependency domain violation(s): found "
          <> displayShow (NE.length errs)
      mapM_ (logError . displayShow) errs
      exitFailure
    Right Ok -> logInfo "All dependency boundary is good!"
    Right (OkWithDiagnostics Diagnostics {..}) -> do
      unless (Map.null usedExceptionalRules) $ do
        logWarn "------------------------------"
        logWarn "* Following exceptional rules are used:"
        iforM_ usedExceptionalRules $ \pkg deps -> do
          let ds = Set.toList deps
          logWarn $
            "    - "
              <> displayShow pkg
              <> " depends on: "
              <> fold
                (intersperse "," $ map displayShow ds)

      unless (Map.null redundantExtraDeps) $ do
        let reduntCount = getSum $ foldMap (Sum . Set.size) redundantExtraDeps
        logWarn "------------------------------"
        logWarn $
          "* "
            <> displayShow reduntCount
            <> " redundant exceptional dependency(s) found:"
        iforM_ redundantExtraDeps $ \pkg deps -> do
          let ds = Set.toList deps
          logWarn $
            "    - "
              <> displayShow pkg
              <> " doesn't depends on: "
              <> fold (intersperse ", " $ map displayShow ds)
              <> "\n"

      logInfo "------------------------------"
      logInfo "All dependency boundary is good, with some additional warning."

defaultMain :: MonadUnliftIO m => BuildInfo -> m ()
defaultMain binfo = do
  logOpts <-
    logOptionsHandle stdout True
      <&> setLogUseTime False
      <&> setLogUseLoc False
  withLogFunc logOpts $ \logFun -> do
    app <- mkSimpleApp logFun Nothing
    runRIO app $
      defaultMainWith binfo =<< liftIO getArgs
