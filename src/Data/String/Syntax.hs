{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.String.Syntax (processFile) where

import Data.Text (Text)
import qualified Data.Text as Text
import Text.Megaparsec (errorBundlePretty, runParser)

import Data.String.Syntax.Internal.Parse (parseHaskellFile)
import Data.String.Syntax.Internal.PostProcess (postProcessFile)

processFile :: FilePath -> Text -> IO Text
processFile path file =
  case runParser parseHaskellFile path file of
    Left e -> errorWithoutStackTrace $ errorBundlePretty e
    Right parsedFile -> pure . addLinePragma . postProcessFile $ parsedFile
  where
    addLine line f = line <> "\n" <> f
    -- this is needed to tell GHC to use original path in error messages
    addLinePragma = addLine $ "{-# LINE 1 \"" <> Text.pack path <> "\" #-}"
