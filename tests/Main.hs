module Main (main) where

import Test.HUnit

import qualified Test.Tasty                       as Tasty
import           Test.Tasty.HUnit                 (testCase)
import           Test.Tasty.QuickCheck            (testProperty)
import qualified Test.Attoparsec

main :: IO ()
main = Tasty.defaultMain tests

tests :: Tasty.TestTree
tests = Tasty.testGroup "root"
    [ Tasty.testGroup "Sample."   sampleTests
    , Tasty.testGroup "Attoparsec." Test.Attoparsec.tests
    ]

sampleTests :: [Tasty.TestTree]
sampleTests = [ testProperty "QuickCheck" $ \x -> const True (x :: Int) == True
              , testCase     "HUnit"      $ True @?= True
              ]
