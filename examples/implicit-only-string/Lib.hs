{-# OPTIONS_GHC -Wno-orphans #-}

module Lib where

import Data.String.Syntax.ImplicitOnlyString (Interpolate (..))
import Data.Text (Text)
import Data.Text qualified as Text

{----- Text -----}

instance Interpolate Text where
  interpolate = Text.unpack
