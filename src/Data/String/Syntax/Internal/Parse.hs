{-# LANGUAGE OverloadedStrings #-}

module Data.String.Syntax.Internal.Parse (
  DelimMarker (..),
  HaskellCode (..),
  HaskellCodeChunk (..),
  HaskellCodeSrc,
  parseHaskellFile,
) where

import Control.Monad (mzero, when)
import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

data DelimMarker = OnlyDelim String | AnyDelim

newtype HaskellCode = HaskellCode [HaskellCodeChunk]
  deriving (Show)

data HaskellCodeChunk
  = UnparsedSource HaskellCodeSrc
  | InterpolatedString Text [Either Text HaskellCodeSrc]
  deriving (Show)

type HaskellCodeSrc = Text

parseHaskellFile :: DelimMarker -> FilePath -> Text -> IO HaskellCode
parseHaskellFile delim path file =
  case runParser (parseHaskellCode delim <* eof) path file of
    Left e -> errorWithoutStackTrace $ errorBundlePretty e
    Right parsedFile -> pure parsedFile

{----- Parser -----}

type Parser = Parsec Void Text

parseHaskellCode :: DelimMarker -> Parser HaskellCode
parseHaskellCode delim = HaskellCode <$> parseCodeChunks
  where
    parseCodeChunks = concat <$> many parseCodeChunk

    parseCodeChunk =
      choice
        [ do
            chunks <- between (string "(") (string ")") parseCodeChunks
            pure $ [UnparsedSource "("] <> chunks <> [UnparsedSource ")"]
        , (:[]) <$> parseStringLiteral delim
        , do
            -- consume any leading space
            spaces1 <- takeWhileP Nothing isSpace
            -- consume characters
            src <- takeWhileP Nothing (\c -> c `notElem` [' ', '(', ')'])
            -- consume any trailing space
            spaces2 <- takeWhileP Nothing isSpace
            -- if we didn't consume anything, explicitly fail
            when (Text.null spaces1 && Text.null src && Text.null spaces2) mzero
            -- then stop, so that we can check for any potential `ident"` tokens
            pure [UnparsedSource $ spaces1 <> src <> spaces2]
        ]

parseStringLiteral :: DelimMarker -> Parser HaskellCodeChunk
parseStringLiteral delim =
  choice
    [ try $ parseStringLiteral' multi interpolate
    | multi <- [True, False]
    , interpolate <- [False, True]
    ]
  where
    parseStringLiteral' multi interpolate = do
      let
        quotes = if multi then "\"\"\"" else "\""
        parseRawStringChunk = parseRawStringChunkUntil interpolate (chunk quotes)

      if interpolate
        then do
          var <-
            case delim of
              OnlyDelim s -> string (Text.pack s)
              AnyDelim -> parseVar

          parts <-
            between (chunk quotes) (chunk quotes) $
              many . choice $
                [ Right <$> parseInterpolatedStringChunk
                , Left <$> parseRawStringChunk
                ]

          -- TODO: normalize multiline strings

          pure $ InterpolatedString var parts
        else do
          parts <-
            between (chunk quotes) (chunk quotes) $
              many parseRawStringChunk

          pure . UnparsedSource $ quotes <> Text.concat parts <> quotes

parseRawStringChunkUntil :: Bool -> Parser delim -> Parser Text
parseRawStringChunkUntil interpolate parseDelim =
  choice
    [ do
        esc <- single '\\'
        c <- anySingle
        pure $ Text.pack [esc, c]
    , notFollowedBy parseDelim *> takeWhile1P Nothing (== '"')
    , takeWhile1P Nothing (`notElem` ['\\', '"'] <> interpolateChar)
    ]
  where
    interpolateChar = if interpolate then ['$'] else []

parseInterpolatedStringChunk :: Parser HaskellCodeSrc
parseInterpolatedStringChunk =
  between (chunk "${") (chunk "}") $
    takeWhile1P Nothing (`notElem` ['}'])

parseVar :: Parser Text
parseVar = do
  mods <- many parseModDot
  var <- parseUnqual
  pure $ Text.intercalate "." (mods <> [var])
  where
    parseModDot = cons <$> upperChar <*> many identChar <* string "."
    parseUnqual =
      choice
        [ cons <$> lowerChar <*> many identChar
        , cons <$> char '_' <*> some identChar
        ]
    identChar = alphaNumChar <|> char '_'
    cons c cs = Text.pack (c : cs)
