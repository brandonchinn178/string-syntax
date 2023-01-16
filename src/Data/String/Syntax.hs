{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.String.Syntax (processFile) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void (Void)
import Text.Megaparsec

processFile :: FilePath -> Text -> IO Text
processFile path file =
  case runParser (parseHaskellCode <* eof) path file of
    Left e -> errorWithoutStackTrace $ errorBundlePretty e
    Right code -> pure . addLinePragma . postProcessCode $ code
  where
    addLine line f = line <> "\n" <> f
    -- this is needed to tell GHC to use original path in error messages
    addLinePragma = addLine $ "{-# LINE 1 \"" <> Text.pack path <> "\" #-}"

newtype HaskellCode = HaskellCode {codeChunks :: [HaskellCodeChunk]}

data HaskellCodeChunk
  = UnparsedSource Text
  | StringLiteral MultiFlag StringLiteral

data StringLiteral
  = RawStringLiteral Text
  | InterpolatedStringLiteral [InterpolatedStringChunk]

data InterpolatedStringChunk
  = RawStringChunk Text
  | InterpolatedStringChunk HaskellCode

data MultiFlag = MultiLine | SingleLine
data InterpolateFlag = Interpolate | NoInterpolate

{----- Parser -----}

type Parser = Parsec Void Text

parseHaskellCode :: Parser HaskellCode
parseHaskellCode = HaskellCode <$> many parseCodeChunk
  where
    parseCodeChunk = choice
      [ parseStringLiteral
      , fmap UnparsedSource $ takeWhile1Not ['s'] ['"']
      ]

parseStringLiteral :: Parser HaskellCodeChunk
parseStringLiteral = choice
  [ parseStringLiteral' MultiLine NoInterpolate
  , parseStringLiteral' MultiLine Interpolate
  , parseStringLiteral' SingleLine Interpolate
  , parseStringLiteral' SingleLine NoInterpolate
  ]
  where
    parseStringLiteral' multi interpolate = do
      let
        delim =
          case multi of
            MultiLine -> "\"\"\""
            SingleLine -> "\""
        parseRawStringChunk = parseRawStringChunkUntil (chunk delim)

      fmap (StringLiteral multi) $
        case interpolate of
          Interpolate -> do
            between (chunk ("s" <> delim)) (chunk delim) $
              fmap InterpolatedStringLiteral . many $
                choice
                  [ InterpolatedStringChunk <$> parseInterpolatedStringChunk
                  , RawStringChunk <$> parseRawStringChunk
                  ]
          NoInterpolate -> do
            between (chunk delim) (chunk delim) $
              RawStringLiteral . Text.concat <$> many parseRawStringChunk

parseRawStringChunkUntil :: Parser delim -> Parser Text
parseRawStringChunkUntil parseDelim = choice
  [ do
      esc <- single '\\'
      c <- anySingle
      pure $ Text.pack [esc, c]
  , notFollowedBy parseDelim *> takeWhile1P Nothing (== '"')
  , takeWhile1P Nothing (`notElem` ['\\', '"', '$'])
  ]

parseInterpolatedStringChunk :: Parser HaskellCode
parseInterpolatedStringChunk =
  between (chunk "${") (chunk "}") $
    HaskellCode . concat <$> many parseCodeChunk
  where
    parseCodeChunk = choice
      [ (: []) <$> parseStringLiteral
      , do
          begin <- chunk "{"
          chunks <- many parseCodeChunk
          end <- chunk "}"
          pure $ [UnparsedSource begin] <> concat chunks <> [UnparsedSource end]
      , (: []) . UnparsedSource <$> takeWhile1Not ['s'] ['"', '{', '}']
      ]

-- | @takeWhile1Not ['a'] ['b']@ parses an optional 'a' at the beginning and
-- any characters afterwards not matching 'a' or 'b'. Fails if parses nothing.
takeWhile1Not :: [Char] -> [Char] -> Parser Text
takeWhile1Not startNoEnd noStartNoEnd = choice
  [ Text.cons <$> oneOf startNoEnd <*> takeWhileP Nothing (`notElem` noEnd)
  , takeWhile1P Nothing (`notElem` noEnd)
  ]
  where
    noEnd = startNoEnd ++ noStartNoEnd

{----- Post-processing -----}

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
