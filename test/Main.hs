import Test.Tasty

import qualified InterpolateTests
import qualified MultilineTests

main :: IO ()
main =
  defaultMain . testGroup "string-syntax" $
    [ MultilineTests.tests
    , InterpolateTests.tests
    ]
