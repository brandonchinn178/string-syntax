module Data.String.Syntax.ImplicitNoBuilder where

class Interpolate a s where
  interpolate :: a -> s

instance Interpolate a a where
  interpolate = id
instance Interpolate Char String where
  interpolate = interpolate . (:[])
instance Interpolate Bool String where
  interpolate = show
instance Interpolate Int String where
  interpolate = show
instance Interpolate Integer String where
  interpolate = show
instance Interpolate Double String where
  interpolate = show
