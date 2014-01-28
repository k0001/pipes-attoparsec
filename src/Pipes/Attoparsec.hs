-- | @pipes@ utilities for incrementally running @attoparsec@-based parsers

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}

module Pipes.Attoparsec (
    -- * Parsing
      parse
    , parsed
    , isEndOfParserInput

    -- * Length Offsets
    , parseL
    , parsedL

    -- * Types
    , ParserInput(..)
    , ParsingError(..)
    ) where

import Control.Exception (Exception)
import Control.Monad.Trans.Error (Error)
import Data.Attoparsec.Types (IResult(..))
import qualified Data.Attoparsec.Types as Attoparsec
import qualified Data.Attoparsec.ByteString
import qualified Data.Attoparsec.Text
import Data.ByteString (ByteString)
import qualified Data.ByteString
import Data.Data (Data, Typeable)
import Data.Monoid (Monoid(mempty))
import Data.Text (Text)
import qualified Data.Text
import Pipes ((>->))
import qualified Pipes.Parse as Pipes
import Pipes.Parse
import qualified Pipes.Prelude as P

-- | Convert an @attoparsec@ parser to a @pipes-parse@ parser
parse
    :: (Monad m, ParserInput t)
    => Attoparsec.Parser t a
    -- ^ 
    -> Pipes.Parser t m (Either ParsingError a)
    -- ^
parse parser = do
    x <- parseL parser
    return $ case x of
        Left   e     -> Left e
        Right (_, a) -> Right a
{-# INLINABLE parse #-}

-- | Convert a stream of 'ParserInput' to a stream of parsed values
parsed
    :: (Monad m, ParserInput t)
    => Attoparsec.Parser t a
    -- ^
    -> Producer t m r
    -- ^
    -> Producer a m (Maybe (ParsingError, Producer t m r))
    -- ^
parsed parser = go
  where
    go p = do
        finished <- lift $ evalStateT isEndOfParserInput p
        if finished
           then return Nothing
           else do (x, p') <- lift $ runStateT (parse parser) p
                   case x of
                       Left   err -> return $ Just (err, p')
                       Right  a   -> yield a >> go p'
{-# INLINABLE parsed #-}

{-| Like 'parse', but also returns the length of input consumed to parse the
    value
-}
parseL
    :: (Monad m, ParserInput t)
    => Attoparsec.Parser t a
    -- ^ 
    -> Pipes.Parser t m (Either ParsingError (Int, a))
    -- ^
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

{-| Like 'parsed', except this tags each parsed value with the length of input
    consumed to parse the value
-}
-- TODO: Refactor with 'parsed'
parsedL
    :: (Monad m, ParserInput t)
    => Attoparsec.Parser t a
    -- ^
    -> Producer t m r
    -- ^
    -> Producer (Int, a) m (Maybe (ParsingError, Producer t m r))
    -- ^
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

{-| Like 'Pipes.Parse.isEndOfInput', except that it also consumes and discards
    leading empty chunks
-}
isEndOfParserInput :: (Monad m, ParserInput t) => Parser t m Bool
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

-- | A class for valid @attoparsec@ input types
class (Eq a, Monoid a) => ParserInput a where
    _parse  :: Attoparsec.Parser a b -> a -> IResult a b
    _length :: a -> Int

instance ParserInput ByteString where
    _parse  = Data.Attoparsec.ByteString.parse
    _length = Data.ByteString.length

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
