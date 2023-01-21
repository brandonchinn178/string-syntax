{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.String.Syntax (processFile) where

import Data.Text (Text)
import Text.Megaparsec (errorBundlePretty, runParser)

import Data.String.Syntax.Internal.Parse (parseHaskellFile)
import Data.String.Syntax.Internal.PostProcess (postProcessFile)

processFile :: FilePath -> Text -> IO Text
processFile path file =
  case runParser parseHaskellFile path file of
    Left e -> errorWithoutStackTrace $ errorBundlePretty e
    Right parsedFile -> pure $ postProcessFile parsedFile
