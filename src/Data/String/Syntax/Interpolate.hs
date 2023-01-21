{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.String.Syntax.Interpolate (
  -- * The Interpolate class
  Interpolate (..),

  -- * The InterpolateValue class
  InterpolateValue (..),

  -- * The InterpolateValueFlag class
  InterpolateValueFlag,
  InterpolateDefault,
  InterpolateOverride,

  -- * The InterpolateValueRunner class
  InterpolateValueRunner (..),
) where

import Data.Proxy (Proxy (..))
import Data.String (IsString, fromString)
import Data.Text (Text)

-- | The 'Interpolate' class is the class of types that can be built
-- from interpolated @s"..."@ strings. For example: 'String', 'Text',
-- 'HTML', 'SqlQuery'.
--
-- Most types that can be represented as an interpolated string have IsString
-- and Semigroup instances, so in this common case, the default implementations
-- are perfectly fine.
--
-- @
-- data MyString = ...
--
-- instance IsString MyString where
--   ...
-- instance Semigroup MyString where
--   ...
--
-- instance Interpolate MyString
-- @
class Interpolate s where
  interpolateRaw :: String -> s -> s
  default interpolateRaw :: (IsString s, Semigroup s) => String -> s -> s
  interpolateRaw s = (fromString s <>)

  interpolateEmpty :: s
  default interpolateEmpty :: IsString s => s
  interpolateEmpty = fromString ""

instance Interpolate String
instance Interpolate Text

-- | The 'InterpolateValueRunner' class is what @${...}@ expressions in
-- interpolated string desugar with. You should generally not need to
-- touch this class.
class Interpolate s => InterpolateValueRunner s a where
  doInterpolate :: a -> s -> s

-- | The 'InterpolateValue' class is the class of types that can be
-- interpolated in @${...}@ expressions.
--
-- If defining an instance for your specific 'Interpolate' type,
-- use 'InterpolateOverride' and define both 'InterpolateValueFlag'
-- and 'InterpolateValue':
--
-- @
-- instance InterpolateValueFlag MyString Int InterpolateOverride
-- instance InterpolateValue InterpolateOverride MyString Int where
--   ...
--
-- instance InterpolateValueFlag MyString MyUser InterpolateOverride
-- instance InterpolateValue InterpolateOverride MyString MyUser where
--   ...
-- @
--
-- If defining an instance for generic 'Interpolate' types (generally only
-- needed for new custom types), use 'InterpolateDefault' and only define
-- 'InterpolateValue', e.g.:
--
-- @
-- instance
--   InterpolateValue InterpolateDefault s String =>
--   InterpolateValue InterpolateDefault s MyUser
--   where
--     interpolatePrec p = interpolatePrec p . show
-- @
class Interpolate s => InterpolateValue flag s a where
  {-# MINIMAL interpolate | interpolatePrec #-}

  interpolate :: Proxy flag -> a -> s
  interpolate proxy a = interpolatePrec proxy 0 a interpolateEmpty

  interpolatePrec :: Proxy flag -> Int -> a -> s -> s
  default interpolatePrec :: Semigroup s => Proxy flag -> Int -> a -> s -> s
  interpolatePrec proxy _ a s = interpolate proxy a <> s

data InterpolateDefault
data InterpolateOverride

-- | This class is a witness of whether @s@ explicitly defines an
-- 'InterpolateValue' instance for @a@, or if the default instance
-- should be used. See 'InterpolateValue'.
class InterpolateValueFlag s a flag | s a -> flag

instance {-# OVERLAPPABLE #-} (flag ~ InterpolateDefault) => InterpolateValueFlag s a flag

-- The only InterpolateValueRunner instance that determines if we should
-- use the default or overridden InterpolateValue instance.
instance
  ( Interpolate s
  , InterpolateValueFlag s a flag
  , InterpolateValue flag s a
  ) =>
  InterpolateValueRunner s a
  where
  doInterpolate = interpolatePrec (Proxy :: Proxy flag) 0

instance
  ( Interpolate s
  , InterpolateValue InterpolateOverride s String
  ) =>
  InterpolateValue InterpolateDefault s Char
  where
  interpolatePrec _ p c = interpolatePrec (Proxy :: Proxy InterpolateOverride) p [c]

instance InterpolateValueFlag String String InterpolateOverride
instance InterpolateValue InterpolateOverride String String where
  interpolatePrec _ _ = (<>)

instance
  ( Interpolate s
  , InterpolateValue InterpolateDefault s a
  , InterpolateValue InterpolateDefault s String
  ) =>
  InterpolateValue InterpolateDefault s [a]
  where
  interpolatePrec proxy _ as = go True as . interpolatePrec proxy 0 "]"
    where
      go _ [] = id
      go isStart (x : xs) =
        interpolatePrec proxy 0 (if isStart then "[" else ", ")
          . interpolatePrec proxy 0 x
          . go False xs

#define GEN_INTERPOLATE(TYPE) \
  instance \
    ( Interpolate s \
    , InterpolateValue InterpolateOverride s String \
    ) => InterpolateValue InterpolateDefault s TYPE where \
    interpolatePrec _ p = interpolatePrec (Proxy :: Proxy InterpolateOverride) p . show

{- FOURMOLU_DISABLE -}
GEN_INTERPOLATE(Bool)
GEN_INTERPOLATE(Int)
GEN_INTERPOLATE(Double)
GEN_INTERPOLATE(Float)
{- FOURMOLU_ENABLE -}
