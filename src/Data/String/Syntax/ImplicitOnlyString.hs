module Data.String.Syntax.ImplicitOnlyString where

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
