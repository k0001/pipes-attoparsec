{-# LANGUAGE OverloadedStrings #-}

module Test.Attoparsec (tests) where

import           Control.Monad
import           Control.Proxy                  ((>->))
import qualified Control.Proxy                  as P
import qualified Control.Proxy.Trans.Attoparsec as PA
import           Data.Attoparsec.Text           as AT
import qualified Data.Text                      as T
import           Data.Maybe
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit

-- | Parses a 'Char' repeated four times.
four :: AT.Parser Char
four = do
   c <- AT.anyChar
   replicateM_ 3 $ char c
   return c

fourPC :: (Monad m, P.Proxy p) => () -> P.Consumer (PA.AttoparsecP T.Text p) T.Text m Char
fourPC () = PA.parsePC four

pfours :: (Monad m, P.Proxy p) => () -> P.Pipe (PA.AttoparsecP T.Text p) T.Text Char m r
pfours () = forever $ P.respond =<< (fourPC >-> P.unitU) ()


type FoursTest = (Bool, String, [T.Text], [Char], Maybe T.Text)

assertFoursTest :: FoursTest -> Assertion
assertFoursTest (ok, _, input, output, mlo) = assert . fromJust $ do
  let sess = PA.runParseK Nothing $ P.fromListS input >-> pfours
  ((epe,mlo'), res) <- P.runWriterT . P.runProxy $ sess >-> P.toListD
  let okMlo = mlo' == mlo
      okErr = either (const $ not ok) (const $ ok) epe
      okRes = res == output
  return $ okMlo && okErr && okRes

foursTests :: [FoursTest]
foursTests =
  [ (True  ,"0 chunk"                 ,[]              ,[]        ,Nothing)
  , (True  ,"1 chunk: Empty"          ,[""]            ,[]        ,Nothing)
  -- ^XXX I'm not sure if this is the desired behavior.
  , (True  ,"1 chunk: One"            ,["aaaa"]        ,['a']     ,Nothing)
  , (True  ,"1 chunk: One twice"      ,["aaaaaaaa"]    ,['a','a'] ,Nothing)
  , (True  ,"1 chunk: Two"            ,["aaaabbbb"]    ,['a','b'] ,Nothing)
  , (True  ,"1 chunk: Partial"        ,["aaaab"]       ,['a']     ,Nothing)
  , (False ,"1 chunk: Wrong"          ,["aaxbb"]       ,[]        ,Just "xbb")
  , (False ,"1 chunk: One then wrong" ,["aaaavz"]      ,['a']     ,Just "z")
  , (False ,"2 chunk: Empty"          ,["",""]         ,[]        ,Nothing)
  -- ^Fails cuz those two empty strings are feeded directly to the Parser.
  , (True  ,"2 chunk: One"            ,["a","aaa"]     ,['a']     ,Nothing)
  , (True  ,"2 chunk: One'"           ,["aa","aa"]     ,['a']     ,Nothing)
  , (True  ,"2 chunk: One''"          ,["aaa","a"]     ,['a']     ,Nothing)
  , (True  ,"2 chunk: One'''"         ,["aaaa",""]     ,['a']     ,Nothing)
  , (True  ,"2 chunk: Two"            ,["aaaa","bbbb"] ,['a','b'] ,Nothing)
  , (False ,"2 chunk: Wrong"          ,["abcd","efgh"] ,[]        ,Just "bcd")
  , (False ,"2 chunk: One then wrong" ,["aaaab","bxz"] ,['a']     ,Just "xz")
  , (True  ,"3 chunk: One"            ,["a","a","aa"]  ,['a']     ,Nothing)
  ]

-- testCaseFoursTest :: FoursTest -> Test
testCaseFoursTest ft@(ok,name,_,_,_) =
  testCase name $ assertFoursTest ft


tests = map testCaseFoursTest foursTests
