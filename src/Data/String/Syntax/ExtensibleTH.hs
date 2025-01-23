{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.String.Syntax.ExtensibleTH where

import Data.String (fromString)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

s :: [Either String (Q Exp)] -> Q Exp
s parts = [| mconcat $(listE $ map go parts) |]
  where
    go = \case
      Left str -> lift str
      Right e -> [| fromString $ interpolate $e |]

class Interpolate a where
  interpolate :: a -> String

instance Interpolate String where
  interpolate = id
instance Interpolate Char where
  interpolate = interpolate . (:[])
instance Interpolate Bool where
  interpolate = show
instance Interpolate Int where
  interpolate = show
instance Interpolate Integer where
  interpolate = show
instance Interpolate Double where
  interpolate = show
