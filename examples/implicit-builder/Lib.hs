{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lib where

import Data.Monoid (Endo (..))
import Data.String.Syntax.ImplicitBuilder
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as TextL
import Data.Text.Lazy.Builder qualified as TextL

import SqlQuery

{----- Text -----}

instance Buildable Text where
  type Builder Text = TextL.Builder
  toBuilder = TextL.fromText
  fromBuilder = TextL.toStrict . TextL.toLazyText

instance Interpolate TextL.Builder Text where
  interpolate = id
instance Interpolate Text Text where
  interpolate = toBuilder
instance Interpolate String Text where
  interpolate = TextL.fromString
instance Interpolate Int Text where
  interpolate = TextL.fromString . show

instance Interpolate Text String where
  interpolate = interpolate . Text.unpack

{----- SqlQuery -----}

newtype SqlQueryBuilder = SqlQueryBuilder (Endo SqlQuery)
  deriving newtype (Semigroup, Monoid)

instance Buildable SqlQuery where
  type Builder SqlQuery = SqlQueryBuilder
  toBuilder q = SqlQueryBuilder (Endo (q <>))
  fromBuilder (SqlQueryBuilder (Endo f)) = f mempty

instance Interpolate SqlQuery SqlQuery where
  interpolate = toBuilder
instance Interpolate String SqlQuery where
  interpolate s = toBuilder SqlQuery{sqlText = "?", sqlValues = [SqlString s]}
instance Interpolate Int SqlQuery where
  interpolate x = toBuilder SqlQuery{sqlText = "?", sqlValues = [SqlInt x]}
