-- | This module provides low-level integration with Attoparsec and is likely
-- to be modified in backwards-incompatible ways in the future.
--
-- Use the "Control.Proxy.Attoparsec" module instead.

module Control.Proxy.Attoparsec.Internal
  ( parseWith
  , parseWithMay
  , parseWithMayNoNullCheck
  , mayInput
  ) where

--------------------------------------------------------------------------------

import Data.Attoparsec.Types             (Parser, IResult(..))
import Prelude                           hiding (null)
import Data.Function                     (fix)
import Data.Monoid                       (mempty)
import Control.Proxy.Attoparsec.Types    (ParserInput(..), ParsingError(..))

--------------------------------------------------------------------------------

-- | Run a parser drawing input from the given monadic action as needed.
parseWith
  :: (Monad m, ParserInput a)
  => m a
  -- ^An action that will be executed to provide the parser with more input
  -- as needed. If the action returns 'mempty', then it's assumed no more
  -- input is available.
  -> Parser a r
  -- ^Parser to run on the given input
  -> m (Either ParsingError r, Maybe a)
  -- ^Either a parser error or a parsed result, together with any leftover.
parseWith refill p = step . parse p =<< refill where
    step (Partial k)  = step . k =<< refill
    step (Done t r)   = return (Right r, mayInput t)
    step (Fail t c m) = return (Left (ParsingError c m), mayInput t)
{-# INLINABLE parseWith #-}


-- | Run a parser drawing input from the given monadic action as needed.
parseWithMay
  :: (Monad m, ParserInput a)
  => m (Maybe a)
  -- ^An action that will be executed to provide the parser with more input
  -- as needed. If the action returns 'Nothing', then it's assumed no more
  -- input is available. 'Just mempty' input is discarded.
  -> Parser a r
  -- ^Parser to run on the given input
  -> m (Either ParsingError r, Maybe a)
  -- ^Either a parser error or a parsed result, together with any leftover.
parseWithMay refill = parseWith . fix $ \loop -> do
    ma <- refill
    case ma of
      Just a
       | null a    -> loop  -- retry on null input
       | otherwise -> return a
      Nothing      -> return mempty
{-# INLINABLE parseWithMay #-}


-- | Like 'parseWithMay', except any of 'Nothing' of 'Just mempty' inputs
-- indicate EOF.
parseWithMayNoNullCheck
  :: (Monad m, ParserInput a)
  => m (Maybe a) -> Parser a r -> m (Either ParsingError r, Maybe a)
parseWithMayNoNullCheck refill = parseWith (return . maybe mempty id =<< refill)
{-# INLINABLE parseWithMayNoNullCheck #-}


-- | Wrap @a@ in 'Just' if not-null. Otherwise, 'Nothing'.
mayInput :: ParserInput a => a -> Maybe a
mayInput = \x -> if null x then Nothing else Just x
{-# INLINABLE mayInput #-}

