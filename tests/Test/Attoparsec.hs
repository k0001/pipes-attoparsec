{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Test.Attoparsec (tests) where

import           Control.Monad
import           Pipes
import qualified Pipes.Lift                         as P
import qualified Pipes.Prelude                      as P
import           Control.Monad.Trans.Error          (runErrorT)
import           Control.Monad.Trans.Writer.Strict  (runWriterT, tell)
import           Pipes.Attoparsec                   (parseMany)
import qualified Data.Attoparsec.Text               as AT
import           Data.Functor.Identity              (runIdentity)
import qualified Data.Text                          as T
import           Test.Framework.Providers.HUnit     (testCase)
import           Test.HUnit                         (Assertion, assert)

-- | Parses a 'Char' repeated four times.
four :: AT.Parser Char
four = do
   c <- AT.anyChar
   replicateM_ 3 $ AT.char c
   return c

type ParseTest = (Bool, String, [T.Text], [Char], [T.Text])


assertFoursTest :: ParseTest -> Assertion
assertFoursTest (ok, _title, input, output, mlo) = assert . runIdentity $ do
  (e,res) <- runWriterT . runErrorT . P.toListM $
                for (P.errorP $ parseMany four $ each input)
                    (\a -> lift . lift $ tell [snd a])
  let (okErr,mlo') = case e of
       Left (_, pmlo') -> (not ok, runIdentity . P.toListM . fmap fst $ P.runWriterP pmlo')
       Right _         -> (ok, [])
  let okMlo = mlo' == mlo
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
  , (False ,"2 chunk: Wrong"            ,["abcd","efgh"]    ,[]        ,["bcd","efgh"])
  , (False ,"2 chunk: One then wrong"   ,["aaaab","bxz"]    ,['a']     ,["xz"])
  , (True  ,"3 chunk: One"              ,["a","a","aa"]     ,['a']     ,[])
  ]

testCaseFoursTest ft@(_,name,_,_,_) =
  testCase ("Fours." ++ name) $ assertFoursTest ft

tests = map testCaseFoursTest foursTests
