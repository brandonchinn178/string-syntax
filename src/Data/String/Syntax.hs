{-# LANGUAGE OverloadedStrings #-}

module Data.String.Syntax (processFile) where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void (Void)
import Text.Megaparsec

processFile :: FilePath -> Text -> IO Text
processFile path file =
  case runParser convertFile path file of
    Left e -> errorWithoutStackTrace $ errorBundlePretty e
    Right file' -> pure $ addLinePragma file'
  where
    addLine line f = line <> "\n" <> f
    -- this is needed to tell GHC to use original path in error messages
    addLinePragma = addLine $ "{-# LINE 1 \"" <> Text.pack path <> "\" #-}"

type Parser = Parsec Void Text

data InterpolationFlag = Interpolate | NoInterpolate
data MultilineFlag = MultiLine | SingleLine

convertFile :: Parser Text
convertFile = manySubstrings parseChunk <* eof
  where
    parseChunk = choice
      [ chunk "\"\"\"" *> convertString MultiLine NoInterpolate
      , chunk "s\"\"\"" *> convertString MultiLine Interpolate
      , chunk "s\"" *> convertString SingleLine Interpolate
      , chunk "\"" *> convertString SingleLine NoInterpolate
      , takeFirstAndWhile (\c -> c /= 's' && c /= '"')
      ]

convertString :: MultilineFlag -> InterpolationFlag -> Parser Text
convertString multi interpolate = do
  str <- parseRestOfString
  _ <- chunk delim
  pure $ "\"" <> postProcess str <> "\""
  where
    delim =
      case multi of
        MultiLine -> "\"\"\""
        SingleLine -> "\""

    -- repeatedly consume any chunk matching:
    --   1. Any escaped character: '\' <char>
    --   2. Any quote characters, as long as it's not the delimiter
    --   3. Any characters except a backslash or quote character
    parseRestOfString =
      manySubstrings . choice $
        [ do
            esc <- single '\\'
            c <- anySingle
            pure (Text.pack [esc, c])
        , notFollowedBy (chunk delim) *> takeWhile1P Nothing (== '"')
        , takeWhile1P Nothing (\c -> c /= '\\' && c /= '"')
        ]

    postProcess = processInterpolate . processMultiline

    processMultiline s =
      case multi of
        SingleLine -> s
        MultiLine -> Text.replace "\n" "\\n" s -- TODO

    processInterpolate s =
      case interpolate of
        NoInterpolate -> s
        Interpolate -> s -- TODO

manySubstrings :: Parser Text -> Parser Text
manySubstrings = fmap Text.concat . many

-- | Parse a single character unconditionally, and then any
-- additional character satisfying the given condition
takeFirstAndWhile :: (Char -> Bool) -> Parser Text
takeFirstAndWhile f = Text.append <$> takeP Nothing 1 <*> takeWhileP Nothing f
