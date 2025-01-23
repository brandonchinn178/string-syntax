{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lib where

import Data.String.Syntax.ExtensibleTH (Interpolate (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import SqlQuery

{----- Text -----}

instance Interpolate Text where
  interpolate = Text.unpack

{----- SqlQuery -----}

sql :: [Either String (Q Exp)] -> Q Exp
sql parts =
  let (texts, vals) = unzip $ map go parts
   in [| SqlQuery $(lift $ concat texts) $(listE $ concat vals) |]
  where
    go = \case
      Left s -> (s, [])
      Right e -> ("?", [[| toSqlValue $e |]])
