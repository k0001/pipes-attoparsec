{-# LANGUAGE OverloadedStrings #-}

module Test.Tutorial where

import           Control.Proxy
import           Control.Proxy.Trans.Either (runEitherK)
import qualified Control.Proxy.Attoparsec.Tutorial as Tut
import           Data.Maybe
import           Test.HUnit
import           Test.Framework.Providers.HUnit (testCase)


test_helloPipe1 input test = fromJust $ do
  let proxy = fromListS input >-> Tut.helloPipe1 >-> toListD
  output <- fmap snd . runWriterT . runProxy $ proxy
  return . assert $ test output

test_helloPipe1_input1 = test_helloPipe1 Tut.input1 test
  where test x = x == fmap Tut.Name ["Kate" , "Mary", "Jeff", "Tom"]

test_helloPipe1_input2 = test_helloPipe1 Tut.input2 test
  where test x = x == fmap Tut.Name ["Amy", "Bob", "JamesHelloHello World"
                                    ,"Ann", "Jean-Luc"]

tests =
    [ testCase "helloPipe1 + input1" test_helloPipe1_input1
    , testCase "helloPipe1 + input2" test_helloPipe1_input2
    ]

