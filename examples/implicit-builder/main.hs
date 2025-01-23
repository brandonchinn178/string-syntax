{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Data.String.Syntax.ImplicitBuilder
import Data.Text (Text)

import Lib ()
import SqlQuery

main :: IO ()
main = do
  let name = "Alice" :: String; age = 30 :: Int; person = Person{..}

  print (s"Hello ${name}! Your age is ${age}" :: String)
  print (s"You are: ${person}" :: String)

  let name2 = "Alice" :: Text
  print (s"Hello ${name2}! Your age is ${age}" :: Text)

  print (s"SELECT * FROM user WHERE name ILIKE ${name} AND age = ${age}" :: SqlQuery)

data Person = Person { name :: String, age :: Int }

instance Interpolate Person String where
  interpolate Person{..} = toBuilder s"Person<${name}, ${age}>"
