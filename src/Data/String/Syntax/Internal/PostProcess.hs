{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.String.Syntax.Internal.PostProcess (
  postProcessFile,
) where

import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text

import Data.String.Syntax.Internal.Parse

postProcessFile :: HaskellFile -> Text
postProcessFile (HaskellFile code) = postProcessCode code

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
  escapeNewlines
    . removeInitialNewline
    . removeCommonWhitespacePrefix
    . collapseLineContinuations
  where
    escapeNewlines = mapRaw (Text.replace "\n" "\\n")
    removeInitialNewline = mapRawFirst (\s -> fromMaybe s $ Text.stripPrefix "\n" s)

    -- TODO
    collapseLineContinuations lit = lit

    -- apply the given function to all raw string chunks
    mapRaw f = \case
      RawStringLiteral s -> RawStringLiteral (f s)
      InterpolatedStringLiteral chunks ->
        InterpolatedStringLiteral . flip map chunks $ \case
          RawStringChunk s -> RawStringChunk (f s)
          -- anything interpolated is ignored in this phase; post processing the interpolated
          -- code happens in convertStringLiteral
          InterpolatedStringChunk code -> InterpolatedStringChunk code

    -- same as mapRaw, except only apply the function to the first string chunk
    mapRawFirst f = \case
      RawStringLiteral s -> RawStringLiteral (f s)
      InterpolatedStringLiteral chunks ->
        InterpolatedStringLiteral $
          case chunks of
            (RawStringChunk x : xs) -> RawStringChunk (f x) : xs
            _ -> chunks

removeCommonWhitespacePrefix :: StringLiteral -> StringLiteral
removeCommonWhitespacePrefix = \case
  RawStringLiteral str ->
    let
      strLines = Text.splitOn "\n" str
      commonPrefix = getCommonPrefix strLines
     in
      RawStringLiteral
        . Text.intercalate "\n"
        . map (rmSpace commonPrefix)
        $ strLines
  InterpolatedStringLiteral chunks ->
    -- TODO
    InterpolatedStringLiteral chunks
  where
    isIndent c = c == ' ' || c == '\t'

    getCommonPrefix strLines =
      let excludeLines =
            filter (not . Text.all isIndent) -- ignore lines that are all whitespace
              . drop 1 -- ignore everything before first newline (i.e. the first line in calculation)
       in case NonEmpty.nonEmpty (excludeLines strLines) of
            Nothing -> 0
            Just strLines' ->
              minimum $ NonEmpty.map (Text.length . Text.takeWhile isIndent) strLines'

    rmSpace n s =
      case Text.uncons s of
        _ | n <= 0 -> s
        Just (' ', s') -> rmSpace (n - 1) s'
        Just ('\t', s')
          | n >= 8 -> rmSpace (n - 8) s'
          | otherwise -> Text.replicate (8 - n) " " <> s'
        _ -> s

convertStringLiteral :: StringLiteral -> Text
convertStringLiteral = \case
  RawStringLiteral s -> "\"" <> s <> "\""
  InterpolatedStringLiteral chunks ->
    Text.intercalate " . " (map postProcessChunk chunks) <> " $ StringSyntax.interpolateEmpty"
  where
    postProcessChunk = \case
      RawStringChunk s -> "StringSyntax.interpolateRaw \"" <> s <> "\""
      InterpolatedStringChunk code -> "StringSyntax.interpolatePrec 0 (" <> postProcessCode code <> ")"
