{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lib where

import Data.String.Syntax.ExtensibleHasClass (HasClass (..), Interpolate (..))
import Data.Text (Text)
import Data.Text qualified as Text

import SqlQuery

{----- Text -----}

instance Interpolate Text where
  interpolate = Text.unpack

{----- SqlQuery -----}

sql :: [Either String (HasClass ToSqlValue)] -> SqlQuery
sql = mconcat . map go
  where
    go :: Either String (HasClass ToSqlValue) -> SqlQuery
    go = \case
      Left s -> SqlQuery s []
      Right (HasClass v) -> SqlQuery "?" [toSqlValue v]
