-- | This module allows you to interleave Attoparsec parsing on input flowing
-- downstream through pipes. This module builds on top of the 'pipes-parse'
-- package, so the functionality exported by that package can be reused here.
--
-- This module is meant to be imported qualified
-- > import qualified Control.Proxy.Attoparsec as PA

module Control.Proxy.Attoparsec
  ( -- * Parsing
    parseD
  , parse
    -- * Types
  , T.ParserInput(T.null)
  , T.ParsingError(..)
  ) where

--------------------------------------------------------------------------------

import           Control.Monad                     (unless)
import qualified Control.Proxy                     as P
import qualified Control.Proxy.Parse               as Pa
import qualified Control.Proxy.Attoparsec.Internal as I
import qualified Control.Proxy.Attoparsec.Types    as T
import qualified Control.Proxy.Trans.Either        as P
import qualified Control.Proxy.Trans.State         as P
import           Data.Attoparsec.Types             (Parser)
import           Data.Foldable                     (mapM_)
import           Prelude                           hiding (mapM_)

--------------------------------------------------------------------------------

-- | Parses one element flowing downstream.
--
-- In case of parsing errors, including EOF, a 'T.ParsingError' exception is
-- thrown in the 'Pe.EitherP' proxy transformer.
--
-- Requests more input from upstream using 'Pa.draw' when needed.
--
-- 'T.null' input chunks from upstream will cause undesired parsing failures.
-- If you are not sure whether your input stream is free from 'T.null' chunks,
-- you can use 'P.filterD' upstream:
--
-- @
-- 'P.filterD' ('not' . 'T.null') 'P.>->' 'parse' ...
-- @
--
-- This proxy is meant to be composed in the 'P.request' category.

-- In case you wonder, skipping 'T.null' inputs manually below wouldn't help if
-- we were trying to parse the tail of the stream and there were just 'T.null'
-- inputs left. That's why we just recommend using
-- @'P.filterD' ('not' . 'T.null')@ upstream, which gives the optimal behavior.
parse
  :: (T.ParserInput a, Monad m, P.Proxy p)
  => Parser a r
  -> () -> P.EitherP T.ParsingError (P.StateP [a] p) () (Maybe a) y' y m r
parse parser = \() -> do
    (er, mlo) <- P.liftP (I.parseWithMayNoNullCheck Pa.draw parser)
    P.liftP (mapM_ Pa.unDraw mlo)
    either P.throw return er
{-# INLINABLE parse #-}


-- | Parses elements flowing downstream until EOF.
--
-- In case of parsing errors, a 'T.ParsingError' exception is thrown in the
-- 'Pe.EitherP' proxy transformer.
--
-- Requests more input from upstream using 'Pa.draw', when needed.
--
-- 'T.null' input chunks from upstream will cause undesired parsing failures.
-- If you are not sure whether your input stream is free from 'T.null' chunks,
-- you can use 'P.filterD' upstream:
--
-- @
-- 'P.filterD' ('not' . 'T.null') 'P.>->' 'parseD' ...
-- @
--
-- This proxy is meant to be composed in the 'P.pull' category.
parseD
  :: (T.ParserInput a, Monad m, P.Proxy p)
  => Parser a b
  -> () -> P.Pipe (P.EitherP T.ParsingError (P.StateP [a] p)) (Maybe a) b m ()
parseD parser = \() -> loop
  where
    loop = do
        eof <- P.liftP Pa.isEndOfInput
        unless eof $ do
          () <- P.respond =<< parse parser ()
          loop
{-# INLINABLE parseD #-}

