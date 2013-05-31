-- | This module provides low-level integration with Attoparsec and is likely
-- to be modified in backwards-incompatible ways in the future.
--
-- Use the "Control.Proxy.Attoparsec" module instead.

module Control.Proxy.Attoparsec.Internal
  ( parseWith
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
  => m (Maybe a)
  -- ^An action that will be executed to provide the parser with more input
  -- as needed. If the action returns 'Nothing', then it's assumed no more
  -- input is available. 'Just BS.empty' input is discarded.
  -> Parser a r
  -- ^Parser to run on the given input
  -> m (Either ParsingError r, Maybe a)
  -- ^Either a parser error or a parsed result, together with any leftover.
parseWith refill p = step . parse p =<< refill' where
    step (Partial k)  = step . k =<< refill'
    step (Done t r)   = return (Right r, mayInput t)
    step (Fail t c m) = return (Left (ParsingError c m), mayInput t)

    -- | Wrap 'refill' return value so that it is suitable for 'Partial'.
    refill' = do
        ma <- refill
        case ma of
          Nothing -> return mempty
          Just a  -> if null a then refill' -- retry on null input
                               else return a
{-# INLINABLE parseWith #-}


-- | Wrap @a@ in 'Just' if not-null. Otherwise, 'Nothing'.
mayInput :: ParserInput a => a -> Maybe a
mayInput = \x -> if null x then Nothing else Just x
{-# INLINABLE mayInput #-}

