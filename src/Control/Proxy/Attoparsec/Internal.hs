{-# LANGUAGE DeriveDataTypeable #-}

-- | This module provides low-level integration with Attoparsec and is likely
-- to be modified in backwards-incompatible ways in the future.
--
-- Use the "Control.Proxy.Attoparsec" modules instead.

module Control.Proxy.Attoparsec.Internal
  ( parseWith
  , parseWithMay
  , mayInput
  ) where

--------------------------------------------------------------------------------

import Data.Attoparsec.Types             (Parser, IResult(..))
import Prelude                           hiding (null)
import Data.Monoid                       (mempty)
import Control.Proxy.Attoparsec.Types    (ParserInput(..), ParsingError(..))

--------------------------------------------------------------------------------

-- | Run a parser drawing input from the given monadic action as needed.
parseWith
  :: (Monad m, ParserInput a)
  => m a
  -- ^ An action that will be executed to provide the parser with more input
  -- as needed. If the action returns `mempty`, then it's assumed no more
  -- input is available.
  -> Parser a b
  -- ^ Optional initial input for the parser.
  -> m (Either ParsingError b, Maybe a)
  -- ^ Either a parser error or a parsed result, together with any leftover.
parseWith refill p = step . parse p =<< refill where
    step (Partial k)  = step . k =<< refill
    step (Done t r)   = return (Right r, mayInput t)
    step (Fail t c m) = return (Left (ParsingError c m), mayInput t)
{-# INLINABLE parseWith #-}


-- | Like 'parseWith', except the given monadic action might return either
-- `Nothing` or `Just mempty` to signal that no more input is available.
parseWithMay :: (Monad m, ParserInput a)
             => m (Maybe a) -> Parser a b -> m (Either ParsingError b, Maybe a)
parseWithMay refill p = parseWith (return . maybe mempty id =<< refill) p
{-# INLINABLE parseWithMay #-}


-- | Wrap @a@ in 'Just' if not-null. Otherwise, 'Nothing'.
mayInput :: ParserInput a => a -> Maybe a
mayInput x | null x    = Nothing
           | otherwise = Just x
{-# INLINABLE mayInput #-}

