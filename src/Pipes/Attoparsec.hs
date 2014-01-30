-- | @pipes@ utilities for incrementally running @attoparsec@-based parsers

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes         #-}

module Pipes.Attoparsec (
    -- * Parsing
      parse
    , parsed
    , isEndOfParserInput

    -- * Length Offsets
    , parseL
    , parsedL

    -- * Types
    , ParserInput
    , ParsingError(..)
    ) where

import           Control.Exception          (Exception)
import           Control.Monad.Trans.Error  (Error)
import qualified Data.Attoparsec.ByteString
import qualified Data.Attoparsec.Text
import           Data.Attoparsec.Types      (IResult (..))
import qualified Data.Attoparsec.Types      as Attoparsec
import           Data.ByteString            (ByteString)
import qualified Data.ByteString
import           Data.Data                  (Data, Typeable)
import           Data.Monoid                (Monoid (mempty))
import           Data.Text                  (Text)
import qualified Data.Text
import           Pipes                      (for, (>->))
import           Pipes.Parse
import qualified Pipes.Parse                as Pipes
import qualified Pipes.Prelude              as P

--------------------------------------------------------------------------------

-- | Convert an @attoparsec@ 'Attoparsec.Parser' to a @pipes-parse@
-- 'Pipes.Parser'.
parse
  :: (Monad m, ParserInput a)
  => Attoparsec.Parser a b                    -- ^ Attoparsec parser
  -> Pipes.Parser a m (Either ParsingError b) -- ^ Pipes parser
parse parser = do
    x <- parseL parser
    return $ case x of
        Left   e     -> Left e
        Right (_, a) -> Right a
{-# INLINABLE parse #-}


-- | Convert a stream of 'ParserInput' to a producer of parsed values.
--
-- This producer returns 'Nothing' when end-of-input is reached successfully,
-- otherwise it returns a 'ParsingError' and the leftovers. The leftovers
-- include any input that it was in the process of being parsed.
parsed
  :: (Monad m, ParserInput a)
  => Attoparsec.Parser a b  -- ^ Attoparsec parser
  -> Producer a m r
  -> Producer b m (Maybe (ParsingError, Producer a m r))
parsed parser p = for (parsedL parser p) (yield . snd)
{-# INLINABLE parsed #-}


-- | Like 'parse', but also returns the length of input consumed to parse the
-- value.
parseL
    :: (Monad m, ParserInput a)
    => Attoparsec.Parser a b
    -> Pipes.Parser a m (Either ParsingError (Int, b))
parseL parser = StateT $ \p -> do
    x <- next (p >-> P.filter (/= mempty))
    case x of
        Left   e      -> go id (_parse parser mempty) (return e) 0
        Right (t, p') -> go (yield t >>) (_parse parser t     ) p' $! _length t
  where
    go diffP iResult p len = case iResult of
        Fail _ ctxs msg -> return (Left (ParsingError ctxs msg), diffP p)
        Partial k       -> do
            x <- next p
            case x of
                Left   e      -> go diffP (k mempty) (return e) len
                Right (t, p') ->
                    go (diffP . (yield t >>)) (k t) p' $! len + _length t
        Done t r        -> return (Right (len - _length t, r), yield t >> p)
{-# INLINABLE parseL #-}


-- | Like 'parsed', except this tags each parsed value with the length of input
-- consumed to parse the value.
parsedL
    :: (Monad m, ParserInput a)
    => Attoparsec.Parser a b    -- ^ Attoparsec parser
    -> Producer a m r
    -> Producer (Int, b) m (Maybe (ParsingError, Producer a m r))
parsedL parser = go
  where
    go p = do
        finished <- lift $ evalStateT isEndOfParserInput p
        if finished
           then return Nothing
           else do (x, p') <- lift $ runStateT (parseL parser) p
                   case x of
                       Left   err -> return $ Just (err, p')
                       Right  a   -> yield a >> go p'
{-# INLINABLE parsedL #-}

-- | Like 'Pipes.Parse.isEndOfInput', except that it also consumes and discards
-- leading empty chunks.
isEndOfParserInput :: (Monad m, ParserInput a) => Parser a m Bool
isEndOfParserInput = go
  where
    go = do
        mt <- draw
        case mt of
            Nothing -> return True
            Just a  ->
                if a == mempty
                then go
                else do
                    unDraw a
                    return False
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

-- | A parsing error report, as provided by Attoparsec's 'Fail'.
data ParsingError = ParsingError
    { peContexts :: [String]  -- ^ Contexts where the parsing error occurred.
    , peMessage  ::  String   -- ^ Parsing error description message.
    } deriving (Show, Read, Eq, Data, Typeable)

instance Exception ParsingError
instance Error     ParsingError
