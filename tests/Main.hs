module Main (main) where

import qualified Test.Attoparsec
import qualified Test.Tasty as Tasty

main :: IO ()
main = Tasty.defaultMain tests

tests :: Tasty.TestTree
tests = Tasty.testGroup "root"
      [ Tasty.testGroup "Attoparsec." Test.Attoparsec.tests
      ]
