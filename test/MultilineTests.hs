module MultilineTests (tests) where

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup "multiline" $
    [ testCase "works" $ do
        let s =
              """
              Line 1
              Line 2
                Line 3
              Line 4
              """
        s @?= "Line 1\nLine 2\n  Line 3\nLine 4\n"
    ]
