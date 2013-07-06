{-# LANGUAGE DeriveDataTypeable #-}

-- | This module provides low-level integration with Attoparsec and is likely
-- to be modified in backwards-incompatible ways in the future.
--
-- Use the stable API exported by the "Pipes.Attoparsec" module instead.

module Pipes.Attoparsec.Internal
  ( -- * Types
    ParsingError(..)
  , ParserInput(null)
    -- * Parsing
  , parseWithMay
  ) where

--------------------------------------------------------------------------------

import           Control.Exception                 (Exception)
import           Control.Monad.Trans.Error         (Error)
import           Data.Attoparsec.Types             (Parser, IResult(..))
import qualified Data.Attoparsec.ByteString        as AB
import qualified Data.Attoparsec.Text              as AT
import qualified Data.ByteString.Char8             as B
import           Data.Data                         (Data, Typeable)
import           Data.Monoid                       (Monoid(mempty))
import qualified Data.Text                         as T
import           Prelude                           hiding (null)

--------------------------------------------------------------------------------

-- | A parsing error report, as provided by Attoparsec's 'Fail'.
data ParsingError = ParsingError
  { peContexts :: [String]  -- ^ Contexts where the parsing error occurred.
  , peMessage  :: String    -- ^ Parsing error description message.
  } deriving (Show, Eq, Data, Typeable)

instance Exception ParsingError where
instance Error     ParsingError where

--------------------------------------------------------------------------------

-- | A class for valid Attoparsec input types: strict 'T.Text' and
-- strict 'B.ByteString'.
class (Monoid a) => ParserInput a where
    -- | Run a 'Parser' with input @a@.
    parse :: Parser a b -> a -> IResult a b
    -- | Tests whether @a@ is empty.
    null :: a -> Bool

instance ParserInput B.ByteString where
    parse      = AB.parse
    null       = B.null

instance ParserInput T.Text where
    parse      = AT.parse
    null       = T.null

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
  -- input is available. @'Just' 'mempty'@ input is discarded.
  -> Parser a r
  -- ^Parser to run on the given input
  -> m (Either ParsingError r, Maybe a)
  -- ^Either a parser error or a parsed result, together with any leftover.
parseWithMay refill p = parseWith loop p
  where
    loop = do
        ma <- refill
        case ma of
          Just a
           | null a    -> loop  -- retry on null input
           | otherwise -> return a
          Nothing      -> return mempty
{-# INLINABLE parseWithMay #-}


-- | Wrap @a@ in 'Just' if not-null. Otherwise, 'Nothing'.
mayInput :: ParserInput a => a -> Maybe a
mayInput x | null x    = Nothing
           | otherwise = Just x
{-# INLINE mayInput #-}


