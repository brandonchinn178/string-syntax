{-# LANGUAGE LambdaCase #-}

import qualified Data.Text.IO as Text
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import System.Environment (getArgs)

import Data.String.Syntax (processFile)

main :: IO ()
main = do
  -- just to be extra sure we don't run into encoding issues
  setLocaleEncoding utf8

  getArgs >>= \case
    -- https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/phases.html#options-affecting-a-haskell-pre-processor
    [fp, input, output] -> Text.readFile input >>= processFile fp >>= Text.writeFile output
    _ -> error "The string-syntax preprocessor does not accept any additional arguments."
