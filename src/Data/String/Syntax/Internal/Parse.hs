{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.String.Syntax.Internal.Parse (
  HaskellFile (..),
  HaskellCode (..),
  HaskellCodeChunk (..),
  StringLiteral (..),
  InterpolatedStringChunk (..),
  MultiFlag (..),
  parseHaskellFile,
) where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void (Void)
import Text.Megaparsec

newtype HaskellFile = HaskellFile HaskellCode

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

parseHaskellFile :: Parser HaskellFile
parseHaskellFile = HaskellFile <$> parseHaskellCode <* eof

parseHaskellCode :: Parser HaskellCode
parseHaskellCode = HaskellCode <$> many parseCodeChunk
  where
    parseCodeChunk =
      choice
        [ parseStringLiteral
        , fmap UnparsedSource $ takeWhile1Not ['s'] ['"']
        ]

parseStringLiteral :: Parser HaskellCodeChunk
parseStringLiteral =
  choice
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
parseRawStringChunkUntil parseDelim =
  choice
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
    parseCodeChunk =
      choice
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
takeWhile1Not startNoEnd noStartNoEnd =
  choice
    [ Text.cons <$> oneOf startNoEnd <*> takeWhileP Nothing (`notElem` noEnd)
    , takeWhile1P Nothing (`notElem` noEnd)
    ]
  where
    noEnd = startNoEnd ++ noStartNoEnd
