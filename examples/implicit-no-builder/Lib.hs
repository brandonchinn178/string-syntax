{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lib where

import Data.Monoid (Endo (..))
import Data.String.Syntax.ImplicitNoBuilder
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as TextL
import Data.Text.Lazy.Builder qualified as TextL

import SqlQuery

{----- Text -----}

instance Interpolate String Text where
  interpolate = Text.pack . interpolate
instance Interpolate Int Text where
  interpolate = Text.pack . interpolate

instance Interpolate Text String where
  interpolate = interpolate . Text.unpack

{----- SqlQuery -----}


instance Interpolate String SqlQuery where
  interpolate s = SqlQuery{sqlText = "?", sqlValues = [SqlString s]}

instance Interpolate Int SqlQuery where
  interpolate x = SqlQuery{sqlText = "?", sqlValues = [SqlInt x]}

