{-# LANGUAGE OverloadedStrings #-}

module Test.Attoparsec (tests) where
import           Debug.Trace

import           Control.Proxy
import           Control.Proxy.Attoparsec             hiding (drop)
import           Control.Proxy.Attoparsec.Control
import           Data.Attoparsec.Text                 (IResult (..), Parser,
                                                       char, satisfy)
import           Data.Char                            (digitToInt)
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                            as T
import           Test.Framework.Providers.QuickCheck2 (testProperty)




-- | Turns [0,6,3,1] into "063" :: T.Text
toDigits :: [Int] -> T.Text
toDigits = mconcat . fmap (T.pack . show)


-- | Attempt to parse input from each yet unsuccesfully parsed suffix of 't'.
parseChunk :: Parser b -> T.Text -> [IResult T.Text b]
parseChunk parser t
  | T.null t  = []
  | otherwise = let x = parse parser t in x : case x of
      Partial _  -> []
      Fail r _ _ -> parseChunk parser (T.tail t)
      Done r x   -> parseChunk parser r


-- | IResult utils
isDone :: IResult a b -> Bool
isDone (Done{}) = True
isDone _        = False

isFail :: IResult a b -> Bool
isFail (Fail{}) = True
isFail _        = False

isPartial :: IResult a b -> Bool
isPartial (Partial{}) = True
isPartial _           = False

doneValue :: IResult a b -> Maybe b
doneValue (Done _ v) = Just v
doneValue _          = Nothing


-- | Split list in chunks of size 'i'.
chunksOf :: Int -> [e] -> [[e]]
chunksOf i ls = map (take i) (build (splitter ls)) where
  build f = f (:) []
  splitter :: [e] -> ([e] -> a -> a) -> a -> a
  splitter [] _ n = n
  splitter l c n  = l `c` splitter (drop i l) c n


-- | Parses any of [100,200..900]
someHundred :: Parser Int
someHundred = do c <- satisfy $ \c -> c >= '1' && c <= '9'
                 char '0' >> char '0'
                 return $ digitToInt c * 100



prop_retryLeftovers :: [[Int]] -> Bool
prop_retryLeftovers xs = fromJust $ do
    ((), out) <- runWriterT . runProxy $
                 fromListS inputChunks
                 >-> parserInputD
                 >-> retryLeftovers
                 >-> parserD someHundred
                 >-> toListD
    expected  <- sequence . fmap doneValue . filter isDone $ parsed
    return $ out == expected
  where
    inputChunks = fmap (toDigits . fmap abs) xs
    inputFull = mconcat inputChunks
    parsed = parseChunk someHundred inputFull



tests = [ testProperty "retryLeftovers" prop_retryLeftovers ]
