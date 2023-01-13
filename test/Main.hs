import Test.Tasty

import qualified MultilineTests
import qualified InterpolateTests

main :: IO ()
main =
  defaultMain . testGroup "string-syntax" $
    [ MultilineTests.tests
    , InterpolateTests.tests
    ]
