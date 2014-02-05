-- | @pipes@ utilities for incrementally running @attoparsec@-based parsers

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes         #-}

module Pipes.Attoparsec (
    -- * Parsing
      parse
    , parsed
    , isEndOfParserInput

    -- ** Including input lenght.
    , parseL
    , parsedL

    -- * Types
    , ParserInput
    , ParsingError(..)
    ) where

import           Control.Exception                (Exception)
import           Control.Monad                    (liftM)
import           Control.Monad.Trans.Error        (Error)
import qualified Control.Monad.Trans.State.Strict as S
import qualified Data.Attoparsec.ByteString
import qualified Data.Attoparsec.Text
import           Data.Attoparsec.Types            (IResult (..))
import qualified Data.Attoparsec.Types            as Attoparsec
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString
import           Data.Data                        (Data, Typeable)
import           Data.Maybe                       (isJust)
import           Data.Monoid                      (Monoid (mempty))
import           Data.Text                        (Text)
import qualified Data.Text
import           Pipes
import qualified Pipes.Parse                      as Pipes (Parser)
import qualified Pipes.Prelude                    as P

--------------------------------------------------------------------------------

-- | Convert an @attoparsec@ 'Attoparsec.Parser' to a @pipes-parse@
-- 'Pipes.Parser'.
parse
  :: (Monad m, ParserInput a)
  => Attoparsec.Parser a b                    -- ^ Attoparsec parser
  -> Pipes.Parser a m (Either ParsingError b) -- ^ Pipes parser
parse parser = do
    x <- parseL parser
    return (case x of
       Left   e     -> Left e
       Right (_, a) -> Right a)
{-# INLINABLE parse #-}


-- | Convert a stream of 'ParserInput' to a producer of parsed values.
--
-- This producer returns 'Nothing' when end-of-input is reached successfully,
-- otherwise it returns a 'ParsingError' and the leftovers. The leftovers
-- include any input that it was in the process of being parsed.
parsed
  :: (Monad m, ParserInput a)
  => Attoparsec.Parser a b  -- ^ Attoparsec parser
  -> Producer a m r         -- ^ Raw input
  -> Producer b m (Either (ParsingError, Producer a m r) r)
parsed parser p = for (parsedL parser p) (\(_, a) -> yield a)
{-# INLINABLE parsed #-}


-- | Like 'parse', but also returns the length of input consumed to parse the
-- value.
parseL
    :: (Monad m, ParserInput a)
    => Attoparsec.Parser a b    -- ^ Attoparsec parser
    -> Pipes.Parser a m (Either ParsingError (Int, b))
parseL parser = S.StateT $ \p0 -> do
    x <- next (p0 >-> P.filter (/= mempty))
    case x of
      Left   e      -> go id (_parse parser mempty) (return e) 0
      Right (t, p1) -> go (yield t >>) (_parse parser t) p1 $! _length t
  where
    go diffP iResult p0 len = case iResult of
      Fail _ c m -> return (Left  (ParsingError c m)  , diffP p0)
      Done t r   -> return (Right (len - _length t, r), yield t >> p0)
      Partial k  -> do
        x <- next p0
        case x of
          Left   e      -> go diffP (k mempty) (return e) len
          Right (t, p1) -> go (diffP . (yield t >>)) (k t) p1 $! len + _length t
{-# INLINABLE parseL #-}


-- | Like 'parsed', except this tags each parsed value with the length of input
-- consumed to parse the value.
parsedL
    :: (Monad m, ParserInput a)
    => Attoparsec.Parser a b    -- ^ Attoparsec parser
    -> Producer a m r           -- ^ Raw input
    -> Producer (Int, b) m (Either (ParsingError, Producer a m r) r)
parsedL parser = go where
    go p0 = do
      mr <- lift $ S.evalStateT isEndOfParserInput' p0
      case mr of
         Just r  -> return (Right r)
         Nothing -> do
            (x, p1) <- lift $ S.runStateT (parseL parser) p0
            case x of
               Left  e -> return (Left (e, p1))
               Right a -> yield a >> go p1
{-# INLINABLE parsedL #-}

-- | Like 'Pipes.Parse.isEndOfInput', except that it also consumes and discards
-- leading empty chunks.
isEndOfParserInput :: (Monad m, ParserInput a) => Pipes.Parser a m Bool
isEndOfParserInput = isJust `liftM` isEndOfParserInput'
{-# INLINABLE isEndOfParserInput #-}

--------------------------------------------------------------------------------

-- | A class for valid @attoparsec@ input types
class (Eq a, Monoid a) => ParserInput a where
    _parse  :: Attoparsec.Parser a b -> a -> IResult a b
    _length :: a -> Int

-- | Strict 'ByteString'.
instance ParserInput ByteString where
    _parse  = Data.Attoparsec.ByteString.parse
    _length = Data.ByteString.length

-- | Strict 'Text'.
instance ParserInput Text where
    _parse  = Data.Attoparsec.Text.parse
    _length = Data.Text.length

--------------------------------------------------------------------------------

-- | A parsing error report, as provided by Attoparsec's 'Fail'.
data ParsingError = ParsingError
    { peContexts :: [String]  -- ^ Contexts where the parsing error occurred.
    , peMessage  ::  String   -- ^ Parsing error description message.
    } deriving (Show, Read, Eq, Data, Typeable)

instance Exception ParsingError
instance Error     ParsingError

--------------------------------------------------------------------------------
-- Internal stuff

-- | Like 'isEndOfParserInput'', except it returns @'Just' r@ if the producer
-- has reached end of input, otherwise 'Nothing'.
isEndOfParserInput'
  :: (Monad m, ParserInput a) => S.StateT (Producer a m r) m (Maybe r)
isEndOfParserInput' = go =<< S.get where
    go p0 = do
      x <- lift (next p0)
      case x of
         Left r -> S.put (return r) >> return (Just r)
         Right (a,p1)
          | a == mempty -> go p1
          | otherwise   -> S.put (yield a >> p1) >> return Nothing
{-# INLINABLE isEndOfParserInput' #-}

