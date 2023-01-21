{- FOURMOLU_DISABLE -}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module InterpolateTests (tests) where

import Data.Proxy (Proxy (..))
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Tasty
import Test.Tasty.HUnit

import Data.String.Syntax.Interpolate (
  Interpolate (..),
  InterpolateDefault,
  InterpolateOverride,
  InterpolateValue (..),
  InterpolateValueFlag,
 )
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
    , testCase "works with nested braces" $ do
        let s = s"person = ${Person{personName = "Alice"}}" :: String
        s @?= "person = <Person Alice>"
    , testCase "escapes interpolation" $ do
        let s = s"n = \${n}" :: String
        s @?= "n = ${n}"
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
    , testCase "interpolates SQL" $ do
        let
          name = "Alice"
          age = 30
          active = False
          sqlInjection = "TRUE; DELETE FROM user"
          sqlSafe = "(foo = 'bar' OR bar = 'foo')" :: SqlQuery
          result =
            s"""
            SELECT * FROM user
            WHERE
              name = ${name} AND
              age = ${age} AND
              active = ${active} AND
              failedInjection = ${sqlInjection} AND
              ${sqlSafe}
            """
          expected =
            SqlQuery
              ( Text.unlines
                  [ "SELECT * FROM user"
                  , "WHERE"
                  , "  name = ? AND"
                  , "  age = ? AND"
                  , "  active = ? AND"
                  , "  failedInjection = ? AND"
                  , "  (foo = 'bar' OR bar = 'foo')"
                  ]
              )
              [ SqlString name
              , SqlInt age
              , SqlBool active
              , SqlString sqlInjection
              ]
        result @?= expected
    ]

{----- Person -----}

data Person = Person { personName :: String }

instance
  (Interpolate s, InterpolateValue InterpolateOverride s String) =>
  InterpolateValue InterpolateDefault s Person
  where
    interpolatePrec _ p (Person name) = interpolatePrec proxy p (s"<Person ${name}>" :: String)
      where
        proxy = Proxy :: Proxy InterpolateOverride

{----- HTML interpolation -----}

data HTML = HTML Text
  deriving (Show, Eq)

instance IsString HTML where
  fromString = HTML . Text.pack

instance Interpolate HTML where
  interpolateRaw s (HTML t) = HTML (Text.pack s <> t)
  interpolateEmpty = HTML ""

instance InterpolateValueFlag HTML HTML InterpolateOverride
instance InterpolateValue InterpolateOverride HTML HTML where
  interpolatePrec _ _ (HTML s) (HTML t) = HTML (s <> t)

instance InterpolateValueFlag HTML String InterpolateOverride
instance InterpolateValue InterpolateOverride HTML String where
  interpolatePrec _ _ s (HTML t) = HTML $ escape (Text.pack s) <> t
    where
      escape = Text.replace "<" "&lt;" . Text.replace ">" "&gt;"

{----- SqlQuery interpolation -----}

data SqlValue
  = SqlString Text
  | SqlInt Int
  | SqlBool Bool
  | SqlList [SqlValue]
  deriving (Show, Eq)

class ToSqlValue a where
  toSqlValue :: a -> SqlValue
instance {-# OVERLAPPING #-} ToSqlValue String where
  toSqlValue = SqlString . Text.pack
instance ToSqlValue Text where
  toSqlValue = SqlString
instance ToSqlValue Int where
  toSqlValue = SqlInt
instance ToSqlValue Bool where
  toSqlValue = SqlBool
instance ToSqlValue a => ToSqlValue [a] where
  toSqlValue = SqlList . map toSqlValue

data SqlQuery = SqlQuery Text [SqlValue]
  deriving (Show, Eq)

instance IsString SqlQuery where
  fromString s = SqlQuery (Text.pack s) []
instance Semigroup SqlQuery where
  SqlQuery s vs <> SqlQuery s' vs' = SqlQuery (s <> s') (vs <> vs')
instance Monoid SqlQuery where
  mempty = SqlQuery "" []

instance Interpolate SqlQuery

instance InterpolateValueFlag SqlQuery SqlQuery InterpolateOverride
instance InterpolateValue InterpolateOverride SqlQuery SqlQuery where
  interpolatePrec _ _ = (<>)

instance InterpolateValueFlag SqlQuery Int InterpolateOverride
instance InterpolateValueFlag SqlQuery Bool InterpolateOverride
instance InterpolateValueFlag SqlQuery Text InterpolateOverride
instance InterpolateValueFlag SqlQuery String InterpolateOverride
instance InterpolateValueFlag SqlQuery a InterpolateOverride => InterpolateValueFlag SqlQuery [a] InterpolateOverride
instance {-# OVERLAPPABLE #-} ToSqlValue a => InterpolateValue InterpolateOverride SqlQuery a where
  interpolate _ a = SqlQuery "?" [toSqlValue a]
