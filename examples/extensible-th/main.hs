{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Data.String.Syntax.ExtensibleTH (Interpolate (..))
import Data.Text (Text)

import Lib

main :: IO ()
main = do
  let name = "Alice" :: String; age = 30 :: Int; person = Person{..}

  print (s"Hello ${name}! Your age is ${age}" :: String)
  print (s"You are: ${person}" :: String)

  let name2 = "Alice" :: Text
  print (s"Hello ${name2}! Your age is ${age}" :: Text)

  -- use our own `sql` function defined in Lib
  print sql"SELECT * FROM user WHERE name ILIKE ${name} AND age = ${age}"

data Person = Person { name :: String, age :: Int }

instance Interpolate Person where
  interpolate Person{..} = s"Person<${name}, ${age}>"
