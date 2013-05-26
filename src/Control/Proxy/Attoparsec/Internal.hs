{-# LANGUAGE DeriveDataTypeable #-}

-- | This module provides low-level integration with Attoparsec and is likely
-- to be modified in backwards-incompatible ways in the future.

module Control.Proxy.Attoparsec.Internal
  ( AttoparsecInput(..)
  , ParserError(..)
  , parseWith
  , parseWithMay
  , mayInput
  ) where

import           Control.Exception                (Exception)
import qualified Data.Attoparsec.ByteString       as ABS
import qualified Data.Attoparsec.Text             as AT
import           Data.Attoparsec.Types            (Parser, IResult(..))
import qualified Data.ByteString                  as BS
import qualified Data.Text                        as T
import           Prelude                          hiding (null)
import           Data.Monoid                      (Monoid, mempty)
import           Data.Typeable                    (Typeable)


data ParserError = ParserError
  { errorContexts :: [String]  -- ^ Contexts where the error occurred.
  , errorMessage  :: String    -- ^ Error message.
  } deriving (Show, Eq, Typeable)

instance Exception ParserError where


-- | A class for valid Attoparsec input types: 'T.Text' and 'BS.ByteString'.
class (Monoid a, Eq a) => AttoparsecInput a where
    -- | Run a 'Parser' with input @a@.
    parse :: Parser a b -> a -> IResult a b
    -- | Tests whether @a@ is empty.
    null :: a -> Bool

instance AttoparsecInput BS.ByteString where
    parse   = ABS.parse
    null    = BS.null

instance AttoparsecInput T.Text where
    parse   = AT.parse
    null    = T.null


-- | Run a parser drawing input from the given monadic action as needed.
parseWith
  :: (Monad m, AttoparsecInput a)
  => m a
  -- ^ An action that will be executed to provide the parser with more input
  -- as needed. If the action returns `mempty`, then it's assumed no more
  -- input is available.
  -> Parser a b
  -- ^ Optional initial input for the parser.
  -> m (Either ParserError b, Maybe a)
  -- ^ Either a parser error or a parsed result, together with any leftover.
parseWith refill p = step . parse p =<< refill where
    step (Partial k)  = step . k =<< refill
    step (Done t r)   = return (Right r, mayInput t)
    step (Fail t c m) = return (Left (ParserError c m), mayInput t)
{-# INLINABLE parseWith #-}

-- | Like 'parseWith', except the given monadic action might return either
-- `Nothing` or `Just mempty` to signal that no more input is available.
parseWithMay :: (Monad m, AttoparsecInput a)
             => m (Maybe a) -> Parser a b -> m (Either ParserError b, Maybe a)
parseWithMay refill = parseWith (return . maybe mempty id =<< refill)
{-# INLINABLE parseWithMay #-}

-- | Wrap @a@ in 'Just' if not-null. Otherwise, 'Nothing'.
mayInput :: AttoparsecInput a => a -> Maybe a
mayInput x | null x    = Nothing
           | otherwise = Just x
{-# INLINABLE mayInput #-}
