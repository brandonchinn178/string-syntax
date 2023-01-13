module InterpolateTests (tests) where

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup "interpolation" $
    [ testCase "works" $ do
        let x = 123 :: Int
            isTrue = True
            name = "Bob"
            s = s"x = ${x}, isTrue = ${isTrue}, name = ${name}"
        s @?= "x = 123, isTrue = True, name = Bob"
    , testCase "works with multiline strings" $ do
        let (x, y) = (1, 2) :: (Int, Int)
            s =
              s"""
              x = ${x}
              y = ${y}
              x + y = ${x + y}
              """
        s @?= "x = 1\ny = 2\nx + y = 3\n"
    ]
