{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE KindSignatures #-}

-- | This module exports common types used throughout
-- @"Control.Proxy.Attoparsec".*@

module Control.Proxy.Attoparsec.Types
  ( -- * Proxy control
    ParserStatus(..)
  , SupplyUse(..)
  , ParserSupply
    -- * Error types
  , BadInput(..)
    -- * Attoparsec integration
  , AttoparsecInput(..)
  , mayInput
  , parseWith
  , parsingWith
  , ParserError(..)
  ) where

import qualified Data.Attoparsec.ByteString as ABS
import qualified Data.Attoparsec.Text       as AT
import           Data.Attoparsec.Types
import qualified Data.ByteString            as BS
import qualified Data.Text                  as T
import           Prelude                    hiding (null, splitAt)


-- | Status of a parsing 'Proxy'.
data ParserStatus a
  -- | There is a 'Parser' running and waiting for more @a@ input.
  --
  -- Receiving @('Resume', a)@ from upstream would feed input @a@ to the 'Parser'
  -- waiting for more input.
  = Parsing
    { psLength :: Int -- ^Length of input consumed so far.
    }
  -- | A 'Parser' has failed parsing.
  --
  -- Receiving @('Resume', a)@ from upstream would feed input @a@ to a newly
  -- started 'Parser'.
  | Failed
    { psLeftover :: a -- ^Input not yet consumed when the failure occurred.
    , psError    :: ParserError -- ^Error found while parsing.
    }
  deriving (Show, Eq)


data ParserError = ParserError
    { errorContexts :: [String]  -- ^ Contexts where the error occurred.
    , errorMessage  :: String    -- ^ Error message.
    } deriving (Show, Eq)


-- | Indicates how should a parsing 'Proxy' use new input.
data SupplyUse
  -- | Start a new parser and fed any new input to it.
  = Start
  -- | Resume feeding any new input to the current parser waiting for input.
  | Resume
  deriving (Eq, Show)


-- | Input supplied to a parsing 'Proxy'.
--
-- The 'SupplyUse' value indicates how the @a@ input chunk should be used.
type ParserSupply a = (SupplyUse, a)


-- | Reasons for an input value to be unnaceptable.
data BadInput
  = InputTooLong
    { itlLength :: Int -- ^ Length of the input.
    }
  | MalformedInput
    { miParserErrror :: ParserError -- ^ Error found while parsing.
    }
  deriving (Show, Eq)


-- | A class for valid Attoparsec input types.
class Eq a => AttoparsecInput a where
    -- | Run a 'Parser' with input @a@.
    parse :: Parser a b -> a -> IResult a b
    -- | Tests whether @a@ is empty.
    null :: a -> Bool
    -- | Take the first @n@ elements from @a@ and skip the rest.
    take :: Int -> a -> a
    take n = fst . splitAt n
    -- | Skips the first @n@ elements from @a@ and returns the rest.
    drop :: Int -> a -> a
    drop n = snd . splitAt n
    -- | Equivalent to @('take' n xs, 'drop' n xs)@.
    splitAt :: Int -> a -> (a,a)
    -- | Number of elements in @a@.
    length :: a -> Int

instance AttoparsecInput BS.ByteString where
    parse = ABS.parse
    null = BS.null
    splitAt = BS.splitAt
    take = BS.take
    drop = BS.drop
    length = BS.length

instance AttoparsecInput T.Text where
    parse = AT.parse
    null = T.null
    splitAt = T.splitAt
    take = T.take
    drop = T.drop
    length = T.length

-- | Wrap @a@ in 'Just' if not-null. Otherwise, 'Nothing'.
mayInput :: AttoparsecInput a => a -> Maybe a
mayInput x | null x    = Nothing
           | otherwise = Just x


--- TODO: find a better home for these functions.

-- | Run a parser with an initial input string, and a monadic action
-- that can supply more input if needed.
parseWith
  :: (Monad m, AttoparsecInput a)
  => m a
  -- ^ An action that will be executed to provide the parser with more input,
  -- if necessary. If the action returns an 'empty', then it's assumed no more
  -- input is available.
  -> Parser a b
  -> Maybe a
  -- ^ Optional initial input for the parser.
  -> m (Either ParserError b, Maybe a)
  -- ^ Either a parser error or a parsed result, together with any leftover.
parseWith refill p Nothing  = parseWith refill p . Just =<< refill
parseWith refill p (Just s) = step $ parse p s
  where
    step (Partial k)  = step . k =<< refill
    step (Done t r)   = return (Right r, mayInput t)
    step (Fail t c m) = return (Left (ParserError c m), mayInput t)


-- | 'parseWith' with the order of arguments changed.
-- 
-- Useful to be used as:
-- 
-- > result <- parsingWith myParser initialInput $ do
-- >   ... return more input ...
parsingWith
  :: (Monad m, AttoparsecInput a)
  => Parser a b -> Maybe a -> m a
  -> m (Either ParserError b, Maybe a)
parsingWith p ms refill = parseWith refill p ms

