{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Development.Guardian.Graph.Adapter.Detection (
  detectAdapter,
  detectAdapterThrow,
  detectFromDir,
  detectFromDomainConfig,
  DetectionFailure (..),
) where

import Control.Monad.Trans.Except (ExceptT (..), catchE, runExceptT, throwE)
import Data.Aeson (Value)
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as AKM
import Development.Guardian.Graph.Adapter.Types
import Path (Path, relfile, (</>))
import Path.IO
import Path.Posix (Dir)
import RIO

data DetectionFailure
  = MultipleAdapterConfigFound
  | BothCabalProjectAndStackYamlFound
  | NeitherCabalProjectNorStackYamlFound
  | NoCustomConfigSpecified
  | MalformedConfigYaml Value
  deriving (Show, Eq, Ord, Generic)

instance Exception DetectionFailure where
  displayException MultipleAdapterConfigFound =
    "Could not determine adapter: dependency-domain.yml contains multiple adapter configs"
  displayException BothCabalProjectAndStackYamlFound =
    "Could not determine adapter: Both cabal.project and stack.yaml found"
  displayException NeitherCabalProjectNorStackYamlFound =
    "Could not determine adapter: Neither cabal.project nor stack.yaml found"
  displayException NoCustomConfigSpecified =
    "Could not determine adapter: config file doesn't include neither cabal or stack"
  displayException (MalformedConfigYaml _va) =
    let kind = case _va of
          J.Object {} -> "an object"
          J.Array {} -> "an array"
          J.String {} -> "a string"
          J.Number {} -> "a number"
          J.Bool {} -> "a boolean value"
          J.Null {} -> "null"
     in "Could not determine adapter: malformed configuration; must be object, but got: " <> kind

detectAdapterThrow :: MonadIO m => Value -> Path b Dir -> m StandardAdapters
detectAdapterThrow val = either throwIO pure <=< detectAdapter val

detectAdapter :: MonadIO m => Value -> Path b Dir -> m (Either DetectionFailure StandardAdapters)
detectAdapter config dir = runExceptT $ do
  ExceptT (detectFromDir dir) `catchE` \case
    NeitherCabalProjectNorStackYamlFound ->
      ExceptT $ pure $ detectFromDomainConfig config
    BothCabalProjectAndStackYamlFound ->
      ExceptT $ pure $ detectFromDomainConfig config
    exc -> throwE exc

{- |
Searching for stack.yaml and cabal.project, chooses that one if exactly one of them is found.
-}
detectFromDir ::
  MonadIO m =>
  Path b Dir ->
  m (Either DetectionFailure StandardAdapters)
detectFromDir dir0 = runExceptT $ do
  dir <- canonicalizePath dir0
  stackThere <- doesFileExist (dir </> [relfile|stack.yaml|])
  cabalThere <- doesFileExist (dir </> [relfile|cabal.project|])
  if
      | stackThere && cabalThere -> throwE BothCabalProjectAndStackYamlFound
      | stackThere -> pure Stack
      | cabalThere -> pure Cabal
      | otherwise -> throwE NeitherCabalProjectNorStackYamlFound

{- |
Detects backend from dependency-domains.yml.
If exactly one of `cabal' or `stack' section is present, prefer it.
-}
detectFromDomainConfig :: Value -> Either DetectionFailure StandardAdapters
detectFromDomainConfig val =
  case val of
    J.Object dic -> do
      let cabalPresent = AKM.member "cabal" dic
          stackPresent = AKM.member "stack" dic
      if
          | cabalPresent && stackPresent -> Left MultipleAdapterConfigFound
          | cabalPresent -> Right Cabal
          | stackPresent -> Right Stack
          | otherwise -> Left NoCustomConfigSpecified
    _ -> Left $ MalformedConfigYaml val
