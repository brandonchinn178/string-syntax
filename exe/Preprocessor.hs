import Data.Maybe (fromMaybe)
import qualified Data.Text.IO as Text
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import System.Environment (getArgs)

import Data.String.Syntax

main :: IO ()
main = do
  -- just to be extra sure we don't run into encoding issues
  setLocaleEncoding utf8

  -- https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/phases.html#options-affecting-a-haskell-pre-processor
  fp : input : output : args <- getArgs

  let mode =
        case args of
          [] -> abort "No mode provided"
          [arg] -> parseMode arg `orAbort` ("Invalid mode: " ++ show arg)
          _ -> abort $ "Unexpected arguments: " ++ show args

  Text.readFile input >>= processFile mode fp >>= Text.writeFile output
  where
    abort = errorWithoutStackTrace
    orAbort x msg = fromMaybe (abort msg) x
