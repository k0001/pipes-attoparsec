-- | @pipes@ utilities for incrementally running @attoparsec@-based parsers.
--
-- This module assumes familiarity with @pipes-parse@, you can learn about it in
-- "Pipes.Parse.Tutorial".

{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE RankNTypes         #-}

module Pipes.Attoparsec (
    -- * Parsing
      parse
    , parsed

    -- ** Including input length
    --
    -- $lengths
    , parseL
    , parsedL

    -- * Utils
    , isEndOfParserInput

    -- * Types
    , ParserInput
    , ParsingError(..)
    ) where

import           Control.Exception                (Exception)
import qualified Control.Monad.Trans.State.Strict as S
import qualified Data.Attoparsec.ByteString
import qualified Data.Attoparsec.Text
import           Data.Attoparsec.Types            (IResult (..))
import qualified Data.Attoparsec.Types            as Attoparsec
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString
import           Data.Data                        (Data, Typeable)
import           Data.Text                        (Text)
import qualified Data.Text
import           Pipes
import qualified Pipes.Parse                      as Pipes (Parser)

--------------------------------------------------------------------------------

-- | Convert an @attoparsec@ 'Attoparsec.Parser' to a @pipes-parse@
-- 'Pipes.Parser'.
--
-- This 'Pipes.Parser' is compatible with the tools from "Pipes.Parse".
--
-- It returns 'Nothing' if the underlying 'Producer' is exhausted, otherwise
-- it attempts to run the given attoparsec 'Attoparsec.Parser' on the underlying
-- 'Producer', possibly failing with 'ParsingError'.
parse
  :: (Monad m, ParserInput a)
  => Attoparsec.Parser a b                            -- ^ Attoparsec parser
  -> Pipes.Parser a m (Maybe (Either ParsingError b)) -- ^ Pipes parser
parse parser = S.StateT $ \p0 -> do
    x <- nextSkipEmpty p0
    case x of
      Left r       -> return (Nothing, return r)
      Right (a,p1) -> step (yield a >>) (_parse parser a) p1
  where
    step diffP res p0 = case res of
      Fail _ c m -> return (Just (Left (ParsingError c m)), diffP p0)
      Done a b   -> return (Just (Right b), yield a >> p0)
      Partial k  -> do
        x <- nextSkipEmpty p0
        case x of
          Left e -> step diffP (k mempty) (return e)
          Right (a,p1) -> step (diffP . (yield a >>)) (k a) p1
{-# INLINABLE parse #-}


-- | Convert a producer of 'ParserInput' to a producer of parsed values.
--
-- This producer returns 'Right' when end-of-input is reached sucessfully,
-- otherwise it returns a 'ParsingError' and the leftovers including
-- the malformed input that couldn't be parsed. You can use 'Pipes.Lift.errorP'
-- to promote the 'Either' return value to an 'Control.Monad.Trans.Error.ErrorT'
-- monad transformer.
parsed
  :: (Monad m, ParserInput a)
  => Attoparsec.Parser a b  -- ^ Attoparsec parser
  -> Producer a m r         -- ^ Raw input
  -> Producer b m (Either (ParsingError, Producer a m r) r)
parsed parser = go
  where
    go p0 = do
      x <- lift (nextSkipEmpty p0)
      case x of
        Left r       -> return (Right r)
        Right (a,p1) -> step (yield a >>) (_parse parser a) p1
    step diffP res p0 = case res of
      Fail _ c m -> return (Left (ParsingError c m, diffP p0))
      Done a b   -> yield b >> go (yield a >> p0)
      Partial k  -> do
        x <- lift (nextSkipEmpty p0)
        case x of
          Left e -> step diffP (k mempty) (return e)
          Right (a,p1) -> step (diffP . (yield a >>)) (k a) p1
{-# INLINABLE parsed #-}

--------------------------------------------------------------------------------
-- $lengths
-- Like the functions above, but these also provide information about
-- the length of input consumed in order to fully parse each value.
--------------------------------------------------------------------------------

-- | Like 'parse', but also returns the length of input consumed to parse the
-- value.
parseL
    :: (Monad m, ParserInput a)
    => Attoparsec.Parser a b                           -- ^ Attoparsec parser
    -> Pipes.Parser a m (Maybe (Either ParsingError (Int, b))) -- ^ Pipes parser
parseL parser = S.StateT $ \p0 -> do
    x <- nextSkipEmpty p0
    case x of
      Left r       -> return (Nothing, return r)
      Right (a,p1) -> step (yield a >>) (_parse parser a) p1 (_length a)
  where
    step diffP res p0 !len = case res of
      Fail _ c m -> return (Just (Left (ParsingError c m)), diffP p0)
      Done a b   -> return (Just (Right (len - _length a, b)), yield a >> p0)
      Partial k  -> do
        x <- nextSkipEmpty p0
        case x of
          Left e -> step diffP (k mempty) (return e) len
          Right (a,p1) -> step (diffP . (yield a >>)) (k a) p1 (len + _length a)
{-# INLINABLE parseL #-}


-- | Like 'parsed', except this tags each parsed value with the length of input
-- consumed to parse the value.
parsedL
    :: (Monad m, ParserInput a)
    => Attoparsec.Parser a b    -- ^ Attoparsec parser
    -> Producer a m r           -- ^ Raw input
    -> Producer (Int, b) m (Either (ParsingError, Producer a m r) r)
parsedL parser = go
  where
    go p0 = do
      x <- lift (nextSkipEmpty p0)
      case x of
        Left r       -> return (Right r)
        Right (a,p1) -> step (yield a >>) (_parse parser a) p1 (_length a)
    step diffP res p0 !len = case res of
      Fail _ c m -> return (Left (ParsingError c m, diffP p0))
      Done a b   -> yield (len - _length a, b) >> go (yield a >> p0)
      Partial k  -> do
        x <- lift (nextSkipEmpty p0)
        case x of
          Left e -> step diffP (k mempty) (return e) len
          Right (a,p1) -> step (diffP . (yield a >>)) (k a) p1 (len + _length a)
{-# INLINABLE parsedL #-}

--------------------------------------------------------------------------------

-- | Like 'Pipes.Parse.isEndOfInput', except that it also consumes and discards
-- leading empty chunks.
isEndOfParserInput :: (Monad m, ParserInput a) => Pipes.Parser a m Bool
isEndOfParserInput = S.StateT $ \p0 -> do
    x <- nextSkipEmpty p0
    case x of
       Left r        -> return (True,  return r)
       Right (a, p1) -> return (False, yield a >> p1)
{-# INLINABLE isEndOfParserInput #-}

--------------------------------------------------------------------------------

-- | A class for valid @attoparsec@ input types
class (Eq a, Monoid a) => ParserInput a where
    _parse  :: Attoparsec.Parser a b -> a -> IResult a b
    _length :: a -> Int

-- | Strict 'ByteString'.
instance ParserInput ByteString where
    _parse  = Data.Attoparsec.ByteString.parse
    {-# INLINE _parse #-}
    _length = Data.ByteString.length
    {-# INLINE _length #-}

-- | Strict 'Text'.
instance ParserInput Text where
    _parse  = Data.Attoparsec.Text.parse
    {-# INLINE _parse #-}
    _length = Data.Text.length
    {-# INLINE _length #-}

--------------------------------------------------------------------------------

-- | A parsing error report, as provided by Attoparsec's 'Fail'.
data ParsingError = ParsingError
    { peContexts :: [String]  -- ^ Contexts where the parsing error occurred.
    , peMessage  ::  String   -- ^ Parsing error description message.
    } deriving (Show, Read, Eq, Data, Typeable)

instance Exception ParsingError

--------------------------------------------------------------------------------
-- Internal stuff

-- | Like 'Pipes.next', except it skips leading 'mempty' chunks.
nextSkipEmpty
  :: (Monad m, Eq a, Monoid a)
  => Producer a m r
  -> m (Either r (a, Producer a m r))
nextSkipEmpty = go where
    go p0 = do
      x <- next p0
      case x of
         Left  _        -> return x
         Right (a,p1)
          | a == mempty -> go p1
          | otherwise   -> return x
{-# INLINABLE nextSkipEmpty #-}

