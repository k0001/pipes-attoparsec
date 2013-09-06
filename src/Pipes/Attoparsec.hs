{-# Language RankNTypes #-}

-- | This module allows you to run Attoparsec parsers on input flowing
-- downstream through pipes, possibly interleaving other stream effects
-- while doing so.
--
-- This module builds on top of the @pipes@ and @pipes-parse@ package and
-- assumes you understand how to use those libraries.

module Pipes.Attoparsec
  ( -- * Parsing
    parse
  , parseMany
  , isEndOfParserInput
    -- * Types
  , I.ParserInput
  , I.ParsingError(..)
  ) where

--------------------------------------------------------------------------------

import           Pipes
import qualified Pipes.Parse                       as Pp
import qualified Pipes.Lift                        as P
import qualified Pipes.Attoparsec.Internal         as I
import qualified Control.Monad.Trans.State.Strict  as S
import           Data.Attoparsec.Types             (Parser)
import           Data.Monoid                       (Monoid(mempty))

--------------------------------------------------------------------------------

-- | Run an Attoparsec 'Parser' on input from the underlying 'Producer',
-- returning either a 'I.ParsingError' on failure, or a pair with the parsed
-- entity together with the length of input consumed in order to produce it.
--
-- Use this function only if 'isEndOfParserInput' returns 'False', otherwise
-- you'll get unexpected parsing errors.
parse
  :: (Monad m, I.ParserInput a)
  => Parser a b  -- ^Attoparsec parser.
  -> S.StateT (Producer a m r) m (Either I.ParsingError (Int, b))
parse attoparser = do
    (eb, mlo) <- I.parseWithDraw attoparser
    case mlo of
      Just lo -> Pp.unDraw lo
      Nothing -> return ()
    return eb
{-# INLINABLE parse #-}

-- | Continuously run an Attoparsec 'Parser' on input from the given 'Producer',
-- sending downstream pairs of each successfully parsed entity together with the
-- length of input consumed in order to produce it.
--
-- This 'Producer' runs until it either runs out of input, in which case it
-- returns @'Right' ()@, or until a parsing failure occurs, in which case
-- it returns a 'Left' providing the 'I.ParsingError' and a 'Producer' with any
-- leftovers.
--
-- Hints:
--
-- * You can use 'P.errorP' to promote the 'Either' return value to an
--   'Control.Monad.Trans.Error.ErrorT' monad transformer, which might be
--   particularly handy if you are trying compose this 'Producer' with another
--   'Proxy' that's not so flexible about the return types it accepts.
--
--   @
--   \\parser src -> 'P.errorP' ('parseMany' parser src)
--      :: ('Monad' m, 'I.ParserInput' a)
--      => 'Parser' a b
--      -> 'Producer' a m r
--      -> 'Producer'' ('Int', b) ('Control.Monad.Trans.Error.ErrorT' ('I.ParsingError', 'Producer' a m r) m) ()
--   @
parseMany
  :: (Monad m, I.ParserInput a)
  => Parser a b       -- ^Attoparsec parser.
  -> Producer a m r   -- ^Producer from which to draw input.
  -> Producer' (Int, b) m (Either (I.ParsingError, Producer a m r) ())
parseMany attoparser src = do
    (me, src') <- P.runStateP src go
    return $ case me of
      Just e  -> Left  (e, src')
      Nothing -> Right ()
  where
    go = do
        eof <- lift isEndOfParserInput
        if eof
          then return Nothing
          else do
            eb <- lift (parse attoparser)
            case eb of
              Left  e -> return (Just e)
              Right b -> yield b >> go

--------------------------------------------------------------------------------

-- | Like 'P.isEndOfInput', except it also consumes and discards leading
-- empty 'I.ParserInput' chunks.
isEndOfParserInput
  :: (I.ParserInput a, Monad m)
  => S.StateT (Producer a m r) m Bool
isEndOfParserInput = do
    ma <- Pp.draw
    case ma of
      Left  _         -> return True
      Right a
        | a == mempty -> isEndOfParserInput
        | otherwise   -> Pp.unDraw a >> return False
{-# INLINABLE isEndOfParserInput #-}

