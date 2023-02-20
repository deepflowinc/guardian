{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Development.Guardian.Graph.Adapter.Cabal.Types (
  CustomPackageOptions (..),
  Cabal,
) where

import Control.Applicative (optional, (<|>))
import Data.Aeson (FromJSON, withObject, (.:))
import qualified Data.Aeson.KeyMap as AKM
import Data.Aeson.Types (FromJSON (..))
import Data.Generics.Labels ()
import Development.Guardian.Graph.Adapter.Types (CustomPackageOptions)
import GHC.Generics (Generic)
import Path

data Cabal deriving (Generic)

data instance CustomPackageOptions Cabal = CabalOptions {projectFile :: Maybe (Path Rel File), update :: Maybe (Either Bool String)}
  deriving (Show, Eq, Ord, Generic)

instance FromJSON (CustomPackageOptions Cabal) where
  parseJSON = withObject "{cabal: ...}" $ \obj ->
    if AKM.member "cabal" obj
      then do
        dic <- obj .: "cabal"
        projectFile <- optional $ dic .: "projectFile"
        update <-
          if AKM.member "update" dic
            then
              fmap Just $
                Left <$> (dic .: "update")
                  <|> Right <$> (dic .: "update")
            else pure Nothing
        pure CabalOptions {..}
      else pure (CabalOptions Nothing Nothing)
