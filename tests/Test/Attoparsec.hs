{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Test.Attoparsec (tests) where

import           Control.Monad
import           Control.Proxy                  ((>->))
import qualified Control.Proxy                  as P
import qualified Control.Proxy.Trans.State      as P
import qualified Control.Proxy.Trans.Either     as P
import qualified Control.Proxy.Trans.Writer     as P
import qualified Control.Proxy.Parse            as Pa
import           Control.Proxy.Attoparsec       (parseD)
import qualified Data.Attoparsec.Text           as AT
import qualified Data.Text                      as T
import           Data.Maybe
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     (Assertion, assert)

-- | Parses a 'Char' repeated four times.
four :: AT.Parser Char
four = do
   c <- AT.anyChar
   replicateM_ 3 $ AT.char c
   return c

type ParseTest = (Bool, String, [T.Text], [Char], [T.Text])


assertFoursTest :: ParseTest -> Assertion
assertFoursTest (ok, _title, input, output, mlo) = assert . fromJust $ do
  let srcS = (Pa.wrap.) $ P.fromListS input
      run = P.runProxy . P.runWriterK . P.runStateK Pa.mempty . P.runEitherK
  ((epe,mlo'), res) <- run $ srcS >-> parseD four
                                  >-> P.liftP . P.liftP . P.toListD
  let okMlo = mlo' == mlo
      okErr = either (const $ not ok) (const $ ok) epe
      okRes = res == output
  return $ okMlo && okErr && okRes

foursTests :: [ParseTest]
foursTests =
  [ (True  ,"0 chunk"                   ,[]                 ,[]        ,[])
  , (True  ,"1 chunk: Empty"            ,[]                 ,[]        ,[])
  , (True  ,"1 chunk: One"              ,["aaaa"]           ,['a']     ,[])
  , (True  ,"1 chunk: One twice"        ,["aaaaaaaa"]       ,['a','a'] ,[])
  , (True  ,"1 chunk: Two"              ,["aaaabbbb"]       ,['a','b'] ,[])
  , (True  ,"1 chunk: Two between null" ,["aaaa","","bbbb"] ,['a','b'] ,[])
  , (False ,"1 chunk: Partial"          ,["aaaab"]          ,['a']     ,[])
  , (False ,"1 chunk: Wrong"            ,["aaxbb"]          ,[]        ,["xbb"])
  , (False ,"1 chunk: One then wrong"   ,["aaaavz"]         ,['a']     ,["z"])
  , (True  ,"2 chunk: Empty"            ,["",""]            ,[]        ,[])
  , (True  ,"2 chunk: Empty then one"   ,["","aaaa"]        ,['a']     ,[])
  , (True  ,"2 chunk: One"              ,["a","aaa"]        ,['a']     ,[])
  , (True  ,"2 chunk: One'"             ,["aa","aa"]        ,['a']     ,[])
  , (True  ,"2 chunk: One''"            ,["aaa","a"]        ,['a']     ,[])
  , (True  ,"2 chunk: One'''"           ,["aaaa",""]        ,['a']     ,[])
  , (True  ,"2 chunk: Two"              ,["aaaa","bbbb"]    ,['a','b'] ,[])
  , (False ,"2 chunk: Wrong"            ,["abcd","efgh"]    ,[]        ,["bcd"])
  , (False ,"2 chunk: One then wrong"   ,["aaaab","bxz"]    ,['a']     ,["xz"])
  , (True  ,"3 chunk: One"              ,["a","a","aa"]     ,['a']     ,[])
  ]

testCaseFoursTest ft@(_,name,_,_,_) =
  testCase ("Fours." ++ name) $ assertFoursTest ft

tests = map testCaseFoursTest foursTests
