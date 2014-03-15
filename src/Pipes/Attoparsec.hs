-- | @pipes@ utilities for incrementally running @attoparsec@-based parsers.
--
-- This module assumes familiarity with @pipes-parse@, you can learn about it in
-- "Pipes.Parse.Tutorial".

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
import           Control.Monad.Trans.Error        (Error)
import qualified Control.Monad.Trans.State.Strict as S
import qualified Data.Attoparsec.ByteString
import qualified Data.Attoparsec.Text
import           Data.Attoparsec.Types            (IResult (..))
import qualified Data.Attoparsec.Types            as Attoparsec
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString
import           Data.Data                        (Data, Typeable)
import           Data.Monoid                      (Monoid (mempty))
import           Data.Text                        (Text)
import qualified Data.Text
import           Pipes
import qualified Pipes.Parse                      as Pipes (Parser)
import qualified Pipes.Prelude                    as P

--------------------------------------------------------------------------------

-- | Convert an @attoparsec@ 'Attoparsec.Parser' to a @pipes-parse@
-- 'Pipes.Parser'.
--
-- This 'Pipes.Parser' is compatible with the tools from "Pipes.Parse".
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
parsed parser p = for (parsedL parser p) (\(_, a) -> yield a)
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
    -> Pipes.Parser a m (Either ParsingError (Int, b)) -- ^ Pipes parser
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
      (mr, p1) <- lift $ S.runStateT atEndOfParserInput p0
      case mr of
         Just r  -> return (Right r)
         Nothing -> do
            (x, p2) <- lift $ S.runStateT (parseL parser) p1
            case x of
               Left  e -> return (Left (e, p2))
               Right a -> yield a >> go p2
{-# INLINABLE parsedL #-}

--------------------------------------------------------------------------------

-- | Like 'Pipes.Parse.isEndOfInput', except that it also consumes and discards
-- leading empty chunks.
isEndOfParserInput :: (Monad m, ParserInput a) => Pipes.Parser a m Bool
isEndOfParserInput = do
    mr <- atEndOfParserInput
    return (case mr of
       Nothing -> False
       Just _  -> True)
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
instance Error     ParsingError

-- | This instance allows using 'Pipes.Lift.errorP' with 'parsed' and 'parsedL'
instance Error (ParsingError, Producer a m r)

--------------------------------------------------------------------------------
-- Internal stuff

-- | Returns @'Just' r@ if the producer has reached end of input, otherwise
-- 'Nothing'.
atEndOfParserInput
  :: (Monad m, ParserInput a) => S.StateT (Producer a m r) m (Maybe r)
atEndOfParserInput = go =<< S.get where
    go p0 = do
      x <- lift (next p0)
      case x of
         Left r -> S.put (return r) >> return (Just r)
         Right (a,p1)
          | a == mempty -> go p1
          | otherwise   -> S.put (yield a >> p1) >> return Nothing
{-# INLINABLE atEndOfParserInput #-}

