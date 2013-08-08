{-# LANGUAGE RankNTypes #-}

-- | This module allows you to run Attoparsec parsers on input flowing
-- downstream through Pipes streams, possibly interleaving other stream
-- effects.
--
-- This Module builds on top of the @pipes-parse@ package and assumes
-- you have read "Pipes.Parse.Tutorial".

module Pipes.Attoparsec
  ( -- * Parsing
    -- $parsing
    parse
  , parseMany
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
import qualified Control.Monad.Trans.Error         as E
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
-- stream effects you must use 'parseOne', otherwise you may use the simpler
-- 'parse'.
--
-- These proxies use the 'E.ErrorT' monad transformer to report parsing errors,
-- you might use any of the facilities exported by "Control.Monad.Trans.Either"
-- to recover from them.


-- | Parses one element flowing downstream.
--
-- * In case of parsing errors, a 'I.ParsingError' exception is thrown in
-- the 'E.ErrorT' monad transformer.
--
-- * Requests more input from upstream using 'Pa.draw' when needed.
--
-- * /Do not/ use this proxy if 'isEndOfParserInput' returns 'True',
-- otherwise you may get unexpected parsing errors.
--
-- Here is an example parsing loop that allows interleaving stream effects
-- together with 'parseOne':
--
-- > loop = do
-- >     eof <- hoist lift $ isEndOfParserInput
-- >     unless eof $ do
-- >         -- 1. Possibly perform some stream effects here.
-- >         -- 2. Parse one element from the stream.
-- >         exampleElement <- parseOne myExampleParser
-- >         -- 3. Do something with exampleElement and possibly perform
-- >         --    some more stream effects.
-- >         -- 4. Start all over again.
-- >         loop
parse
  :: (Monad m, I.ParserInput a)
  => Parser a b
  -> E.ErrorT I.ParsingError (S.StateT (Producer a m r) m) b
parse parser = do
    (er, mlo) <- lift (I.parseWithMay Pa.draw parser)
    lift (mapM_ Pa.unDraw mlo)
    either E.throwError return er
{-# INLINABLE parse #-}


-- | Parses consecutive elements flowing downstream until end of input.
--
-- * In case of parsing errors, a 'I.ParsingError' exception is thrown in the
-- 'E.ErrorT' monad transformer.
--
-- * Requests more input from upstream using 'Pa.draw' when needed.
--
-- -- * Empty input chunks flowing downstream will be discarded.
parseMany
  :: (Monad m, I.ParserInput a)
  => Parser a b
  -> Producer b (E.ErrorT I.ParsingError (S.StateT (Producer a m r) m)) ()
parseMany parser = hoist lift Pa.input >-> loop
  where
    loop = do
        eof <- lift (lift isEndOfParserInput)
        unless eof $ do
            () <- yield =<< lift (parse parser)
            loop
{-# INLINABLE parseMany #-}

--------------------------------------------------------------------------------

-- -- | Like 'Pa.isEndOfInput', except it also consumes and discards leading
-- -- empty 'I.ParserInput' chunks.
isEndOfParserInput
  :: (I.ParserInput a, Monad m)
  => S.StateT (Producer a m r) m Bool
isEndOfParserInput = fix $ \loop -> do
    ma <- Pa.draw
    case ma of
      Just a
       | I.null a  -> loop
       | otherwise -> Pa.unDraw a >> return False
      Nothing      -> return True
{-# INLINABLE isEndOfParserInput #-}

