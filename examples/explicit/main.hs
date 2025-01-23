{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Text (Text)
import Data.Text qualified as Text

import Lib

main :: IO ()
main = do
  let name = "Alice" :: String; age = 30 :: Int; person = Person{..}

  -- no inference issues, even with overloaded strings
  print s"Hello ${name}! Your age is ${show age}"
  print s"You are: ${renderPerson person}"

  -- still no inference issues, inferred from expressions
  let name2 = "Alice" :: Text
  print s"Hello ${name2}! Your age is ${Text.pack $ show age}"

  -- still no inference issues, inferred from use of sqlVal
  print s"SELECT * FROM user WHERE name ILIKE ${sqlVal name} AND age = ${sqlVal age}"

data Person = Person { name :: String, age :: Int }

renderPerson :: Person -> String
renderPerson Person{..} = s"Person<${name}, ${show age}>"
