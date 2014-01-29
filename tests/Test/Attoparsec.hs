{-# LANGUAGE OverloadedStrings #-}

module Test.Attoparsec (tests) where

import Control.Monad (replicateM_)
import Data.Maybe (isNothing)
import Data.Text (Text)
import Data.Functor.Identity (runIdentity)
import Control.Monad.Trans.Writer.Strict  (runWriterT, tell)
import qualified Data.Attoparsec.Text as AT
import Pipes (for, each, lift, runEffect)
import Pipes.Prelude (toListM)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase)
import Test.HUnit (Assertion, assert)
import Pipes.Attoparsec (parsed)

-- | Parses a 'Char' repeated four times.
four :: AT.Parser Char
four = do
   c <- AT.anyChar
   replicateM_ 3 $ AT.char c
   return c

type ParseTest = (Bool, String, [Text], [Char], [Text])

assertFoursTest :: ParseTest -> Assertion
assertFoursTest (ok, _title, input, output, mlo) =
    assert $ res == output
          && isNothing e == ok
          && mlo' == mlo
  where
    (e, res) = runIdentity . runWriterT . runEffect
             $ for (parsed four $ each input)
                   (\c -> lift $ tell [c])
    mlo' = case e of
               Nothing -> []
               Just (_,pmlo') -> fst . runIdentity
                                     . runWriterT
                                     $ toListM pmlo'

foursTests :: [ParseTest]
foursTests =
  [ (True  ,"0 chunk"                   ,[]                 ,[]        ,[])
  , (True  ,"1 chunk: Empty"            ,[]                 ,[]        ,[])
  , (True  ,"1 chunk: One"              ,["aaaa"]           ,['a']     ,[])
  , (True  ,"1 chunk: One twice"        ,["aaaaaaaa"]       ,['a','a'] ,[])
  , (True  ,"1 chunk: Two"              ,["aaaabbbb"]       ,['a','b'] ,[])
  , (True  ,"1 chunk: Two between null" ,["aaaa","","bbbb"] ,['a','b'] ,[])
  , (False ,"1 chunk: Partial"          ,["aaaab"]          ,['a']     ,["b"])
  , (False ,"1 chunk: Wrong"            ,["aaxbb"]          ,[]        ,["aaxbb"])
  , (False ,"1 chunk: One then wrong"   ,["aaaavz"]         ,['a']     ,["vz"])
  , (True  ,"2 chunk: Empty"            ,["",""]            ,[]        ,[])
  , (True  ,"2 chunk: Empty then one"   ,["","aaaa"]        ,['a']     ,[])
  , (True  ,"2 chunk: One"              ,["a","aaa"]        ,['a']     ,[])
  , (True  ,"2 chunk: One'"             ,["aa","aa"]        ,['a']     ,[])
  , (True  ,"2 chunk: One''"            ,["aaa","a"]        ,['a']     ,[])
  , (True  ,"2 chunk: One'''"           ,["aaaa",""]        ,['a']     ,[])
  , (True  ,"2 chunk: Two"              ,["aaaa","bbbb"]    ,['a','b'] ,[])
  , (False ,"2 chunk: Wrong"            ,["abcd","efgh"]    ,[]        ,["abcd","efgh"])
  , (False ,"2 chunk: One then wrong"   ,["aaaab","bxz"]    ,['a']     ,["b","bxz"])
  , (True  ,"3 chunk: One"              ,["a","a","aa"]     ,['a']     ,[])
  ]

testCaseFoursTest :: (Bool, [Char], [Text], [Char], [Text]) -> TestTree
testCaseFoursTest ft@(_,name,_,_,_) =
  testCase ("Fours." ++ name) $ assertFoursTest ft

tests :: [TestTree]
tests = map testCaseFoursTest foursTests
