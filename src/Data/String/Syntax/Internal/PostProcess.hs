{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.String.Syntax.Internal.PostProcess (postProcessCode) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text

import Data.String.Syntax.Internal.Parse

postProcessCode :: HaskellCode -> Text
postProcessCode = Text.concat . map fromCodeChunk . codeChunks
  where
    fromCodeChunk = \case
      UnparsedSource s -> s
      StringLiteral multi lit -> convertStringLiteral . postProcessMultiline' multi $ lit

    postProcessMultiline' = \case
      SingleLine -> id
      MultiLine -> postProcessMultiline

postProcessMultiline :: StringLiteral -> StringLiteral
postProcessMultiline =
  mapRaw (Text.replace "\n" "\\n")
    . removeInitialNewline
    . removeCommonWhitespacePrefix
    . collapseLineContinuations
  where
    mapRaw f = \case
      RawStringLiteral s -> RawStringLiteral (f s)
      InterpolatedStringLiteral chunks ->
        InterpolatedStringLiteral . flip map chunks $ \case
          RawStringChunk s -> RawStringChunk (f s)
          -- anything interpolated is ignored in this phase; post processing the interpolated
          -- code happens in convertStringLiteral
          InterpolatedStringChunk code -> InterpolatedStringChunk code

    removeInitialNewline lit =
      let strip s = fromMaybe s $ Text.stripPrefix "\n" s
       in case lit of
            RawStringLiteral s -> RawStringLiteral (strip s)
            InterpolatedStringLiteral chunks ->
              InterpolatedStringLiteral $
                case chunks of
                  (RawStringChunk x : xs) -> RawStringChunk (strip x) : xs
                  _ -> chunks

    removeCommonWhitespacePrefix lit = lit

    collapseLineContinuations lit = lit

convertStringLiteral :: StringLiteral -> Text
convertStringLiteral = \case
  RawStringLiteral s -> "\"" <> s <> "\""
  InterpolatedStringLiteral chunks ->
    Text.intercalate " . " (map postProcessChunk chunks) <> " $ StringSyntax.interpolateEmpty"
  where
    postProcessChunk = \case
      RawStringChunk s -> "StringSyntax.interpolateRaw \"" <> s <> "\""
      InterpolatedStringChunk code -> "StringSyntax.interpolatePrec 0 (" <> postProcessCode code <> ")"
