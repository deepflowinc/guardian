{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Pattern (
  Pattern (),
  concretePrefix,
  parsePattern,
  match,
  patternRE,
) where

import Data.Aeson (FromJSON (..))
import qualified Data.Aeson as J
import Data.Foldable (fold)
import qualified Data.Foldable as F
import Data.Functor.Compose (Compose (..))
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe, isJust)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Text.Regex.Applicative.Text (RE', (<|>))
import qualified Text.Regex.Applicative.Text as RE

data Token = Literal !Text | Wildcard
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable)

newtype Pattern = Pattern {getTokens :: [Token]}
  deriving (Eq, Ord, Generic)
  deriving newtype (Hashable)

instance Show Pattern where
  showsPrec _ (Pattern toks) = foldl (.) id $ map go toks
    where
      go (Literal t) = showString $ T.unpack $ T.replace "*" "\\*" t
      go Wildcard = showChar '*'

-- >>> "hoo\\*bar" :: Pattern
-- /hoo\*bar/

concretePrefix :: Pattern -> Text
{-# INLINE concretePrefix #-}
concretePrefix =
  fold
    . Compose
    . takeWhile isJust
    . map (\case Literal t -> Just t; _ -> Nothing)
    . getTokens

instance IsString Pattern where
  fromString = fromMaybe (error "fromString(Pattern): parse failed!") . parsePattern . T.pack
  {-# INLINE fromString #-}

compilePattern :: Pattern -> RE' [Text]
{-# INLINE compilePattern #-}
compilePattern = traverse go . getTokens
  where
    go (Literal t) = RE.string t
    go Wildcard = textOf $ RE.many RE.anySym

textOf :: RE' a -> RE' Text
textOf = fmap snd . RE.withMatched

instance FromJSON Pattern where
  parseJSON = J.withText "pattern" $ \text ->
    maybe (fail $ "Invalid pattern: " <> show text) pure $
      parsePattern text
  {-# INLINE parseJSON #-}

{- |
Matches against the pattern.

>>> match "foo*bar" "foobar"
Just "foobar"

>>> match "foo.bar.*" "foo.bar.buz"
Just "foo.bar.buz"

>>> match "foo\\*bar" "foobar"
Nothing
-}
match :: Pattern -> Text -> Maybe Text
{-# INLINE match #-}
match = fmap (fmap mconcat) . RE.match . compilePattern

parsePattern :: Text -> Maybe Pattern
parsePattern = RE.match patternRE

patternRE :: RE' Pattern
patternRE =
  Pattern . F.toList
    <$> RE.reFoldl
      RE.Greedy
      ( \case
          Seq.Empty -> Seq.singleton
          ls@(_ :|> Wildcard) -> \case
            Wildcard -> ls
            r -> ls :|> r
          ls@(ls0 :|> Literal l) -> \case
            Literal r -> ls0 :|> Literal (l <> r)
            r -> ls :|> r
      )
      Seq.empty
      ( Literal <$> textOf (RE.many $ RE.psym (`notElem` ['\\', '*']))
          <|> Wildcard <$ RE.sym '*'
          <|> Literal . T.singleton <$ RE.sym '\\' <*> RE.anySym
      )
