{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}

-- | This module provides low-level integration with Attoparsec and is likely
-- to be modified in backwards-incompatible ways in the future.
--
-- Use the stable API exported by the "Pipes.Attoparsec" module instead.

module Pipes.Attoparsec.Internal
  ( -- * Types
    ParsingError(..)
  , ParserInput
    -- * Parsing
  , parseWithDraw
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
import           Pipes                             (Producer)
import qualified Pipes.Parse                       as Pp
import           Prelude                           hiding (null, length)

--------------------------------------------------------------------------------

-- | A parsing error report, as provided by Attoparsec's 'Fail'.
data ParsingError = ParsingError
  { peContexts :: [String]  -- ^ Contexts where the parsing error occurred.
  , peMessage  :: String    -- ^ Parsing error description message.
  } deriving (Show, Read, Eq, Data, Typeable)

instance Exception ParsingError
instance Error     ParsingError

--------------------------------------------------------------------------------

instance (Monad m, ParserInput a) => Error (ParsingError, Producer a m r)

--------------------------------------------------------------------------------

-- | A class for valid Attoparsec input types: strict 'T.Text' and
-- strict 'B.ByteString'.
class (Eq a, Monoid a) => ParserInput a where
    -- | Run a 'Parser' with input @a@.
    parse :: Parser a b -> a -> IResult a b
    -- | Length of @a@.
    length :: a -> Int

instance ParserInput B.ByteString where
    parse      = AB.parse
    length     = B.length

instance ParserInput T.Text where
    parse      = AT.parse
    length     = T.length

--------------------------------------------------------------------------------

-- | Run a parser drawing input from the given monadic action as needed.
parseWithRaw
  :: (Monad m, ParserInput a)
  => m a
  -- ^An action that will be executed to provide the parser with more input
  -- as needed. If the action returns 'mempty', then it's assumed no more
  -- input is available.
  -> Parser a r
  -- ^Parser to run on the given input
  -> m (Either ParsingError (Int, r), Maybe a)
  -- ^Either a parser error or a pair of a result and the parsed input length,
  -- as well as an any leftovers.
parseWithRaw refill p = refill >>= \a -> step (length a) (parse p a)
  where
    step !len res = case res of
        Partial k  -> refill >>= \a -> step (len + length a) (k a)
        Done t r   -> return (Right (len - length t, r), mayInput t)
        Fail t c m -> return (Left  (ParsingError c m) , mayInput t)
{-# INLINABLE parseWithRaw #-}

-- | Run a parser drawing input from the underlying 'Producer'.
parseWithDraw
  :: (Monad m, ParserInput a)
  => Parser a b
  -- ^Parser to run on the given input
  -> Pp.StateT (Producer a m r) m (Either ParsingError (Int, b), Maybe a)
  -- ^Either a parser error or a pair of a result and the parsed input length,
  -- as well as an any leftovers.
parseWithDraw = parseWithRaw refill
  where
    refill = do
        ra <- Pp.draw
        case ra of
          Left  _         -> return mempty
          Right a
            | a == mempty -> refill
            | otherwise   -> return a
{-# INLINABLE parseWithDraw #-}

--------------------------------------------------------------------------------

-- | Wrap @a@ in 'Just' if not-null. Otherwise, 'Nothing'.
mayInput :: ParserInput a => a -> Maybe a
mayInput x | x == mempty = Nothing
           | otherwise   = Just x
{-# INLINE mayInput #-}
