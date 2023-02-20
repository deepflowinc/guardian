{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Development.Guardian.Graph.Adapter.Stack.Types (
  Stack,
  CustomPackageOptions (..),
) where

import Data.Aeson (FromJSON (parseJSON))
import qualified Data.Aeson as J
import Data.Text (Text)
import Development.Guardian.Graph.Adapter.Types
import GHC.Generics (Generic)

data Stack

newtype instance CustomPackageOptions Stack = StackOptions {stackOptions :: [Text]}
  deriving (Show, Eq, Ord, Generic)

instance FromJSON (CustomPackageOptions Stack) where
  parseJSON = J.withObject "{stack: }" $ \obj -> do
    stack <- obj J..:? "stack"
    case stack of
      Nothing -> pure $ StackOptions []
      Just dic -> StackOptions <$> dic J..:? "options" J..!= []
