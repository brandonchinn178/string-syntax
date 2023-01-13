{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.String.Syntax.Interpolate (
  Interpolate (..),
  InterpolateValue (..),
) where

import Data.String (IsString, fromString)
import Data.Text (Text)

class Interpolate s where
  interpolateRaw :: String -> s -> s
  default interpolateRaw :: (IsString s, Monoid s) => String -> s -> s
  interpolateRaw s = (fromString s <>)

  interpolateEmpty :: s
  default interpolateEmpty :: IsString s => s
  interpolateEmpty = fromString ""

class Interpolate s => InterpolateValue s a where
  {-# MINIMAL interpolate | interpolatePrec #-}

  interpolate :: a -> s
  interpolate a = interpolatePrec 0 a interpolateEmpty

  interpolatePrec :: Int -> a -> s -> s
  default interpolatePrec :: Monoid s => Int -> a -> s -> s
  interpolatePrec _ a s = interpolate a <> s

instance Interpolate String
instance Interpolate Text

instance Interpolate s => InterpolateValue s Char where
  interpolatePrec p c = interpolatePrec p [c]

instance {-# OVERLAPPING #-} InterpolateValue String String where
  interpolatePrec _ = (<>)

instance (Interpolate s, InterpolateValue s a) => InterpolateValue s [a] where
  interpolatePrec _ as = go True as . interpolatePrec 0 "]"
    where
      go _ [] = id
      go isStart (x:xs) =
        interpolatePrec 0 (if isStart then "[" else ", ")
        . interpolatePrec 0 x
        . go False xs

#define GEN_INTERPOLATE(TYPE) \
  instance (Interpolate s, InterpolateValue s String) => InterpolateValue s TYPE where \
    interpolatePrec p = interpolatePrec p . show

GEN_INTERPOLATE(Bool)
GEN_INTERPOLATE(Int)
GEN_INTERPOLATE(Double)
GEN_INTERPOLATE(Float)
