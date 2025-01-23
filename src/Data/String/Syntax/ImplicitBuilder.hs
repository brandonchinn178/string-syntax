{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Data.String.Syntax.ImplicitBuilder where

import Data.Monoid (Endo (..))

-- | @Buildable s@ allows @s@ to be built from an interpolated string.
--
-- Laws:
--   * @fromBuilder . toBuilder === id@
--   * @toBuilder . fromBuilder === id@
class Monoid (Builder s) => Buildable s where
  type Builder s = b | b -> s
  toBuilder :: s -> Builder s
  fromBuilder :: Builder s -> s

newtype StringBuilder = StringBuilder (Endo String)
  deriving newtype (Semigroup, Monoid)

instance Buildable String where
  type Builder String = StringBuilder
  toBuilder s = StringBuilder (Endo (s ++))
  fromBuilder (StringBuilder (Endo f)) = f ""

-- | Laws:
--     * interpolate @s @s = toBuilder
--     * interpolate @(Builder s) @s = id
class Buildable s => Interpolate a s where
  interpolate :: a -> Builder s

instance Interpolate StringBuilder String where
  interpolate = id
instance Interpolate String String where
  interpolate = toBuilder
instance Interpolate Char String where
  interpolate = interpolate . (:[])
instance Interpolate Bool String where
  interpolate = StringBuilder . Endo . shows
instance Interpolate Int String where
  interpolate = StringBuilder . Endo . shows
instance Interpolate Integer String where
  interpolate = StringBuilder . Endo . shows
instance Interpolate Double String where
  interpolate = StringBuilder . Endo . shows
