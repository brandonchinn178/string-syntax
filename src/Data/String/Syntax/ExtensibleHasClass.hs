module Data.String.Syntax.ExtensibleHasClass where

import Data.String (IsString, fromString)

data HasClass c = forall a. c a => HasClass a

s :: (Monoid s, IsString s) => [Either s (HasClass Interpolate)] -> s
s = foldMap (either id (\(HasClass x) -> fromString $ interpolate x))

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
