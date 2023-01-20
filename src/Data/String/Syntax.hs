{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.String.Syntax (processFile) where

import Data.Text (Text)
import qualified Data.Text as Text
import Text.Megaparsec (eof, errorBundlePretty, runParser)

import Data.String.Syntax.Internal.Parse (parseHaskellCode)
import Data.String.Syntax.Internal.PostProcess (postProcessCode)

processFile :: FilePath -> Text -> IO Text
processFile path file =
  case runParser (parseHaskellCode <* eof) path file of
    Left e -> errorWithoutStackTrace $ errorBundlePretty e
    Right code -> pure . addLinePragma . postProcessCode $ code
  where
    addLine line f = line <> "\n" <> f
    -- this is needed to tell GHC to use original path in error messages
    addLinePragma = addLine $ "{-# LINE 1 \"" <> Text.pack path <> "\" #-}"
