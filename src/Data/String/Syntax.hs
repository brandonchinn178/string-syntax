{-# LANGUAGE OverloadedStrings #-}

module Data.String.Syntax (processFile) where

import Data.Text (Text)
import qualified Data.Text as Text

processFile :: FilePath -> Text -> IO Text
processFile path file = pure $ addLinePragma file
  where
    addLine line f = line <> "\n" <> f
    -- this is needed to tell GHC to use original path in error messages
    addLinePragma = addLine $ "{-# LINE 1 \"" <> Text.pack path <> "\" #-}"
