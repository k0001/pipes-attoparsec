-- | This module allows you to interleave Attoparsec parsing on input flowing
-- downstream through pipes. This module builds on top of the 'pipes-parse'
-- package, so the functionality exported by that package can be reused here.

module Control.Proxy.Attoparsec
  ( -- * Parsing
    parse
  , parseD
    -- * Exports
  , module Control.Proxy.Attoparsec.Types
  ) where

--------------------------------------------------------------------------------

import           Control.Monad                     (unless)
import qualified Control.Proxy                     as P
import qualified Control.Proxy.Parse               as Pa
import qualified Control.Proxy.Attoparsec.Internal as I
import           Control.Proxy.Attoparsec.Types    (ParserInput, ParsingError)
import qualified Control.Proxy.Trans.Either        as P
import qualified Control.Proxy.Trans.State         as P
import           Data.Attoparsec.Types             (Parser)
import           Data.Foldable                     (mapM_)
import           Prelude                           hiding (mapM_)

--------------------------------------------------------------------------------

-- | Parses one element flowing downstream.
--
-- In case of parsing errors, a 'ParsingError' exception is thrown in the
-- 'Pe.EitherP' proxy transformer.
--
-- Requests more input from upstream using 'Pa.draw', when needed.
--
-- This proxy is meant to be composed in the 'P.request' category.
parse
  :: (ParserInput a, Monad m, P.Proxy p)
  => Parser a r
  -> () -> P.EitherP ParsingError (P.StateP [a] p) () (Maybe a) y' y m r
parse parser = \() -> do
    (er, mlo) <- P.liftP (I.parseWith Pa.draw parser)
    P.liftP (mapM_ Pa.unDraw mlo)
    either P.throw return er
{-# INLINABLE parse #-}


-- | Parses elements flowing downstream until EOF.
--
-- In case of parsing errors, a 'ParsingError' exception is thrown in the
-- 'Pe.EitherP' proxy transformer.
--
-- Requests more input from upstream using 'Pa.draw', when needed.
--
-- This proxy is meant to be composed in the 'P.pull' and 'P.push' categories.
parseD
  :: (ParserInput a, Monad m, P.Proxy p)
  => Parser a b
  -> () -> P.Pipe (P.EitherP ParsingError (P.StateP [a] p)) (Maybe a) b m ()
parseD parser = \() -> loop where
    loop = do
      () <- P.respond =<< parse parser ()
      eof <- P.liftP $ Pa.isEndOfInput
      unless eof loop
{-# INLINABLE parseD #-}
