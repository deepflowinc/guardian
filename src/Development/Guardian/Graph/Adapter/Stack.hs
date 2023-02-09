{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Development.Guardian.Graph.Adapter.Stack (
  buildPackageGraphM,
  buildPackageGraph,
  Stack,
  CustomPackageOptions (..),
) where

import qualified Algebra.Graph as G
import Control.Applicative ((<**>))
import Data.Aeson (FromJSON (parseJSON))
import qualified Data.Aeson as J
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import Development.Guardian.Graph.Adapter.Types
import Development.Guardian.Types (Overlayed (Overlayed, getOverlayed), PackageGraph)
import qualified Development.Guardian.Types as Guard
import Distribution.Simple (unPackageName)
import GHC.Generics (Generic)
import Options.Applicative (helper)
import qualified Options.Applicative as Opt
import Path (fromAbsDir)
import Path.IO (withCurrentDir)
import Stack.Build.Source (loadLocalPackage)
import Stack.Options.GlobalParser (globalOptsFromMonoid, globalOptsParser)
import Stack.Options.Utils (GlobalOptsContext (OuterGlobalOpts))
import Stack.Prelude (RIO, toList, view)
import qualified Stack.Prelude as Stack
import Stack.Runners (ShouldReexec (NoReexec), withConfig, withDefaultEnvConfig, withRunnerGlobal)
import Stack.Types.Build (LocalPackage)
import Stack.Types.Config (HasBuildConfig, HasSourceMap (sourceMapL))
import Stack.Types.Package (LocalPackage (..), Package (..))
import qualified Stack.Types.Package as Stack
import Stack.Types.SourceMap (SourceMap (..))

data Stack

newtype instance CustomPackageOptions Stack = StackOptions {stackOptions :: [Text]}
  deriving (Show, Eq, Ord, Generic)

instance FromJSON (CustomPackageOptions Stack) where
  parseJSON = J.withObject "{stack: }" $ \obj -> do
    stack <- obj J..:? "stack"
    case stack of
      Nothing -> pure $ StackOptions []
      Just dic -> StackOptions <$> dic J..:? "options" J..!= []

localPackageToPackage :: LocalPackage -> Package
localPackageToPackage lp =
  fromMaybe (lpPackage lp) (lpTestBench lp)

{- | Resolve the direct (depth 0) external dependencies of the given local packages (assumed to come from project packages)

Stolen from @stack@ and further simplified.
-}
projectPackageDependencies ::
  [LocalPackage] -> [(Stack.PackageName, Set Stack.PackageName)]
projectPackageDependencies locals =
  map
    ( \lp ->
        let pkg = localPackageToPackage lp
         in (Stack.packageName pkg, deps pkg)
    )
    locals
  where
    deps pkg =
      Set.intersection localNames (packageAllDeps pkg)
    localNames = Set.fromList $ map (Stack.packageName . lpPackage) locals

buildPackageGraph :: PackageGraphOptions Stack -> IO PackageGraph
buildPackageGraph PackageGraphOptions {customOptions = StackOptions {..}, ..} = do
  withCurrentDir targetPath $ do
    let pInfo =
          Opt.info
            (globalOptsParser (fromAbsDir targetPath) OuterGlobalOpts Nothing <**> helper)
            mempty
        cliOpts =
          "--skip-ghc-check"
            : concat
              [ ["--test", "--no-run-tests"]
              | tests components
              ]
              <> concat
                [ ["--bench", "--no-run-benchmarks"]
                | benchmarks components
                ]
            ++ map T.unpack stackOptions
    Just gopt <-
      mapM (globalOptsFromMonoid False) $
        Opt.getParseResult $
          Opt.execParserPure (Opt.prefs mempty) pInfo cliOpts

    withRunnerGlobal gopt $
      withConfig NoReexec $
        withDefaultEnvConfig buildPackageGraphM

buildPackageGraphM ::
  (HasSourceMap env, HasBuildConfig env) =>
  RIO env PackageGraph
buildPackageGraphM = do
  sourceMap <- view sourceMapL
  locals <- mapM loadLocalPackage $ toList $ smProject sourceMap
  let gr = projectPackageDependencies locals
  pure $
    getOverlayed $
      foldMap
        ( \(fromStackPackageName -> pkg, deps) ->
            foldMap (Overlayed . G.edge pkg . fromStackPackageName) deps
        )
        gr

fromStackPackageName :: Stack.PackageName -> Guard.PackageName
fromStackPackageName = fromString . unPackageName
