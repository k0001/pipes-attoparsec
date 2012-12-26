{-# LANGUAGE OverloadedStrings #-}

module Test.Tutorial where

import           Control.Proxy
import           Control.Proxy.Attoparsec
import           Control.Proxy.Attoparsec.Tutorial
import           Control.Proxy.Trans.Either
import           Data.Maybe
import           Test.Framework.Providers.HUnit    (testCase)
import           Test.HUnit                        hiding (test)


testPipe pipe input test = fromJust $ do
  let proxy = fromListS input >-> pipe >-> toListD
  output <- runWriterT . runProxy . runEitherK $ proxy
  return . assert $ test output


test_helloPipe1_input1 = testPipe helloPipe1 input1 test
  where test (Right _, x) = x == fmap Name ["Kate" , "Mary", "Jeff", "Tom"]
        test _            = False

test_helloPipe1_input2 = testPipe helloPipe1 input2 test
  where test (Right _, x) = x == fmap Name [ "Amy", "Bob"
                                           , "JamesHelloHello World" , "Ann"
                                           , "Jean-Luc" ]
        test _            = False


test_helloPipe2_input2 = testPipe helloPipe2 input2 test
   where test (Right _, x) = x == fmap Name [ "Amy" ,"Tim", "Bob"
                                            , "JamesHelloHello World", "Jon"
                                            , "Ann" , "Jean-Luc" ]
         test _            = False

test_helloPipe3_input2 = testPipe helloPipe3 input2 test
   where test (Left (MalformedInput _), [Name "Amy"]) = True
         test _                                       = False


test_helloPipe4_input2 = testPipe helloPipe4 input2 test
   where test (Left (InputTooLong 11), [Name "Amy", Name "Bob"]) = True
         test _                                                  = False


test_helloPipe5_input2 = testPipe helloPipe5 input2 test
   where test (Left (InputTooLong 11),
               [Name "Amy", Name "Tim", Name "Bob"]) = True
         test _                                      = False

test_helloPipe6_input1 = testPipe helloPipe6 input1 test
   where test (Right _, [Name "Kate", Name "Mary"]) = True
         test _                                     = False


tests =
    [ testCase "helloPipe1 + input1" test_helloPipe1_input1
    , testCase "helloPipe1 + input2" test_helloPipe1_input2
    , testCase "helloPipe2 + input2" test_helloPipe2_input2
    , testCase "helloPipe3 + input2" test_helloPipe3_input2
    , testCase "helloPipe4 + input2" test_helloPipe4_input2
    , testCase "helloPipe5 + input2" test_helloPipe5_input2
    , testCase "helloPipe6 + input1" test_helloPipe6_input1
    ]

