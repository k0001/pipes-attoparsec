{-# LANGUAGE RankNTypes #-}

-- | This module allows you to run Attoparsec parsers on input flowing
-- downstream through Pipes streams, possibly interleaving other stream
-- effects.
--
-- This module builds on top of the @pipes-parse@ package and assumes
-- you have read "Pipes.Parse.Tutorial".

module Pipes.Attoparsec
  ( -- * Parsing
    -- $parsing
    parse
  , parseD
  , isEndOfParserInput
    -- * Types
  , I.ParserInput(I.null)
  , I.ParsingError(..)
  ) where

--------------------------------------------------------------------------------

import           Control.Monad                     (unless)
import           Pipes
import qualified Pipes.Parse                       as Pa
import qualified Pipes.Attoparsec.Internal         as I
import qualified Control.Monad.Trans.Either        as E
import qualified Control.Monad.Trans.State.Strict  as S
import           Data.Attoparsec.Types             (Parser)
import           Data.Foldable                     (mapM_)
import           Data.Function                     (fix)
import           Prelude                           hiding (mapM_)

--------------------------------------------------------------------------------
-- $parsing
--
-- There are two basic parsing facilities exported by this module, and choosing
-- between them is easy: If you need to interleave Attoparsec parsing with other
-- stream effects you must use 'parse', otherwise you may use the simpler
-- 'parseD'.
--
-- These proxies use the 'E.EitherT' monad transformer to report parsing errors,
-- you might use any of the facilities exported by "Control.Monad.Trans.Either"
-- to recover from them.


-- | Parses one element flowing downstream.
--
-- * In case of parsing errors, a 'I.ParsingError' exception is thrown in
-- the 'E.EitherT' monad transformer.
--
-- * Requests more input from upstream using 'Pa.draw' when needed.
--
-- * /Do not/ use this proxy if 'isEndOfParserInput' returns 'True',
-- otherwise you may get unexpected parsing errors.
--
-- Here is an example parsing loop that allows interleaving stream effects
-- together with 'parse':
--
-- > loop = do
-- >     eof <- hoist lift $ isEndOfParserInput
-- >     unless eof $ do
-- >         -- 1. Possibly perform some stream effects here.
-- >         -- 2. Parse one element from the stream.
-- >         exampleElement <- parse myExampleParser
-- >         -- 3. Do something with exampleElement and possibly perform
-- >         --    some more stream effects.
-- >         -- 4. Start all over again.
-- >         loop
parse
  :: (I.ParserInput a, Monad m)
  => Parser a r -- ^Attoparsec parser to run on the input stream.
  -> Consumer (Maybe a) (E.EitherT I.ParsingError (S.StateT [a] m)) r
    -- ^Proxy compatible with the facilities provided by "Pipes.Parse".
parse parser = do
    (er, mlo) <- hoist lift $ I.parseWithMay Pa.draw parser
    hoist lift $ mapM_ Pa.unDraw mlo
    either (lift . E.left) return er
{-# INLINABLE parse #-}


-- | Parses consecutive elements flowing downstream until end of input.
--
-- * In case of parsing errors, a 'I.ParsingError' exception is thrown in the
-- 'E.EitherT' monad transformer.
--
-- * Requests more input from upstream using 'Pa.draw' when needed.
--
-- * Empty input chunks flowing downstream will be discarded.
parseD
  :: (I.ParserInput a, Monad m)
  => Parser a b -- ^Attoparsec parser to run on the input stream.
  -> () -> Pipe (Maybe a) b (E.EitherT I.ParsingError (S.StateT [a] m)) ()
    -- ^Proxy compatible with the facilities provided by "Pipes.Parse".
parseD parser = \() -> loop
  where
    loop = do
        eof <- hoist lift $ isEndOfParserInput
        unless eof $ do
          () <- respond =<< parse parser
          loop
{-# INLINABLE parseD #-}

--------------------------------------------------------------------------------

-- | Like 'Pa.isEndOfInput', except it also consumes and discards leading
-- empty 'I.ParserInput' chunks.
isEndOfParserInput
  :: (I.ParserInput a, Monad m)
  => Consumer (Maybe a) (S.StateT [a] m) Bool
isEndOfParserInput = fix $ \loop -> do
    ma <- Pa.draw
    case ma of
      Just a
       | I.null a  -> loop
       | otherwise -> Pa.unDraw a >> return False
      Nothing      -> return True
{-# INLINABLE isEndOfParserInput #-}

