{- FOURMOLU_DISABLE -}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module InterpolateTests (tests) where

import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Tasty
import Test.Tasty.HUnit

import Data.String.Syntax.Interpolate (Interpolate (..), InterpolateValue (..))
import qualified Data.String.Syntax.Interpolate as StringSyntax

tests :: TestTree
tests =
  testGroup "interpolation" $
    [ testCase "works" $ do
        let x = 123 :: Int
            isTrue = True
            name = "Bob" :: String
            s = s"x = ${x}, isTrue = ${isTrue}, name = ${name}" :: String
        s @?= "x = 123, isTrue = True, name = Bob"
    , testCase "works with multiline strings" $ do
        let (x, y) = (1, 2) :: (Int, Int)
            s =
              s"""
              x = ${x}
              y = ${y}
              x + y = ${x + y}
              """ :: String
        s @?= "x = 1\ny = 2\nx + y = 3\n"
    , testCase "works with nested string" $ do
        let s = s"name = ${"Alice" :: String}" :: String
        s @?= "name = Alice"
    , testCase "works with nested interpolated string" $ do
        let n = 12 :: Int
            s = s"n = ${s"${n}" :: String}" :: String
        s @?= "n = 12"
    , testCase "interpolates HTML" $ do
        let
          name = "Alice" :: String
          age = 30 :: Int
          active = False
          htmlInjection = "<script>alert('evil!')</script>" :: String
          htmlSafe = "<b>Hello</b>" :: HTML
          result =
            s"""
            <html>
              <body>
                <p>Name: ${name}</p>
                <p>Age: ${age}</p>
                <p>Active: ${active}</p>
                <p>Failed injection: ${htmlInjection}</p>
                <p>Safe HTML: ${htmlSafe}</p>
              </body>
            </html>
            """
          expected =
            HTML . Text.unlines $
              [ "<html>"
              , "  <body>"
              , "    <p>Name: Alice</p>"
              , "    <p>Age: 30</p>"
              , "    <p>Active: False</p>"
              , "    <p>Failed injection: &lt;script&gt;alert('evil!')&lt;/script&gt;</p>"
              , "    <p>Safe HTML: <b>Hello</b></p>"
              , "  </body>"
              , "</html>"
              ]
        result @?= expected
    ]

{----- HTML interpolation -----}

data HTML = HTML Text
  deriving (Show, Eq)

instance IsString HTML where
  fromString = HTML . Text.pack

instance Interpolate HTML where
  interpolateRaw s (HTML t) = HTML (Text.pack s <> t)
  interpolateEmpty = HTML ""

instance InterpolateValue HTML HTML where
  interpolatePrec _ (HTML s) (HTML t) = HTML (s <> t)

instance {-# OVERLAPPING #-} InterpolateValue HTML String where
  interpolatePrec _ s (HTML t) = HTML $ escape (Text.pack s) <> t
    where
      escape = Text.replace "<" "&lt;" . Text.replace ">" "&gt;"

-- TODO: SqlQuery interpolation
