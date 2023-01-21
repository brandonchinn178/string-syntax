{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.String.Syntax.Internal.PostProcess (
  postProcessFile,
) where

import Data.Bifunctor (first)
import Data.Foldable (toList)
import Data.List (intercalate)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromMaybe)
import qualified Data.Sequence as Seq
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
            RawStringChunk x : xs -> RawStringChunk (f x) : xs
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
    let
      chunkLines = breakInterpolatedLines chunks
      commonPrefix = getCommonPrefix $ map fst chunkLines
     in
      InterpolatedStringLiteral
        . concatInterpolatedLines
        . map (first $ rmSpace commonPrefix)
        $ chunkLines
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

-- | Break the given chunks into a list of lines, where each line is
-- represented as a pair containing the leading RawStringChunk and the chunks
-- making up the rest of the line.
breakInterpolatedLines :: [InterpolatedStringChunk] -> [(Text, [InterpolatedStringChunk])]
breakInterpolatedLines = map splitPrefix . splitLines Seq.empty
  where
    splitLines currLine = \case
      [] -> [toList currLine]
      chunk : rest ->
        case chunk of
          RawStringChunk s
            | l : ls <- Text.splitOn "\n" s ->
                let
                  (midLines, lastLine) = splitLast (map RawStringChunk ls)
                  thisLine = toList (currLine Seq.|> RawStringChunk l)
                  midLineChunks = map (: []) midLines
                 in
                  thisLine : midLineChunks ++ splitLines (Seq.fromList lastLine) rest
          _ -> splitLines (currLine Seq.|> chunk) rest

    splitPrefix =
      let fromRaw = \case
            RawStringChunk s -> Just s
            InterpolatedStringChunk{} -> Nothing
       in first Text.concat . spanMaybe fromRaw

    splitLast xs =
      case NonEmpty.nonEmpty xs of
        Nothing -> ([], [])
        Just xs' -> (NonEmpty.init xs', [NonEmpty.last xs'])

    spanMaybe :: (a -> Maybe b) -> [a] -> ([b], [a])
    spanMaybe f = \case
      a : as | Just b <- f a -> first (b :) $ spanMaybe f as
      as -> ([], as)

-- | The inverse of breakInterpolatedLines
concatInterpolatedLines :: [(Text, [InterpolatedStringChunk])] -> [InterpolatedStringChunk]
concatInterpolatedLines = mergeRawStringChunks . intercalate [RawStringChunk "\n"] . map mergePrefix
  where
    mergePrefix (prefix, chunks) = RawStringChunk prefix : chunks

    mergeRawStringChunks = \case
      RawStringChunk a : RawStringChunk b : rest -> mergeRawStringChunks $ RawStringChunk (a <> b) : rest
      chunk : rest -> chunk : mergeRawStringChunks rest
      [] -> []

convertStringLiteral :: StringLiteral -> Text
convertStringLiteral = \case
  RawStringLiteral s -> "\"" <> s <> "\""
  InterpolatedStringLiteral chunks ->
    Text.intercalate " . " (map postProcessChunk chunks) <> " $ StringSyntax.interpolateEmpty"
  where
    postProcessChunk = \case
      RawStringChunk s -> "StringSyntax.interpolateRaw \"" <> s <> "\""
      InterpolatedStringChunk code -> "StringSyntax.doInterpolate (" <> postProcessCode code <> ")"
