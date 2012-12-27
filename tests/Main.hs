module Main where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit

import qualified Test.Tutorial
import qualified Test.Attoparsec

main = defaultMain tests

tests =
    [ testGroup "Sample"   sampleTests
    , testGroup "Tutorial" Test.Tutorial.tests
    , testGroup "Attoparsec" Test.Attoparsec.tests
    ]

sampleTests = [ testProperty "QuickCheck" $ \x -> const True (x :: Int) == True
              , testCase     "HUnit"      $ True @?= True
              ]
