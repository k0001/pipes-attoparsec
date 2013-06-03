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
  , I.ParserInput
  , I.ParsingError(..)
  ) where

--------------------------------------------------------------------------------

import           Control.Monad                     (unless)
import qualified Control.Proxy                     as P
import qualified Control.Proxy.Parse               as Pa
import qualified Control.Proxy.Attoparsec.Internal as I
import qualified Control.Proxy.Trans.Either        as P
import qualified Control.Proxy.Trans.State         as P
import           Data.Attoparsec.Types             (Parser)
import           Data.Foldable                     (mapM_)
import           Prelude                           hiding (mapM_)

--------------------------------------------------------------------------------

-- | Parses one element flowing downstream.
--
-- In case of parsing errors, including EOF, a 'I.ParsingError' exception is
-- thrown in the 'Pe.EitherP' proxy transformer.
--
-- Requests more input from upstream using 'Pa.draw' when needed. Empty input
-- chunks from upstream will be eagerly consumed and discarded.
--
-- This proxy is meant to be composed in the 'P.request' category.
parse
  :: (I.ParserInput a, Monad m, P.Proxy p)
  => Parser a r
  -> () -> P.EitherP I.ParsingError (P.StateP [a] p) () (Maybe a) y' y m r
parse parser = \() -> do
    (er, mlo) <- P.liftP (I.parseWithMay Pa.draw parser)
    P.liftP $ dropNulls >> mapM_ Pa.unDraw mlo
    either P.throw return er
{-# INLINABLE parse #-}


-- | Parses elements flowing downstream until EOF.
--
-- In case of parsing errors, a 'I.ParsingError' exception is thrown in the
-- 'Pe.EitherP' proxy transformer.
--
-- Requests more input from upstream using 'Pa.draw', when needed. Empty input
-- chunks from upstream will be eagerly consumed and discarded.
--
-- This proxy is meant to be composed in the 'P.pull' category.
parseD
  :: (I.ParserInput a, Monad m, P.Proxy p)
  => Parser a b
  -> () -> P.Pipe (P.EitherP I.ParsingError (P.StateP [a] p)) (Maybe a) b m ()
parseD parser = \() -> loop
  where
    loop = do
        eof <- P.liftP Pa.isEndOfInput
        unless eof $ do
            () <- P.respond =<< parse parser ()
            loop
{-# INLINABLE parseD #-}


-- | Consume and discard all leading 'I.null' input chunks.
--
-- > dropNulls = (Pa.passWhile I.null P.>-> P.evalStateK Pa.mempty Pa.skipAll) ()
dropNulls
  :: (I.ParserInput a, Monad m, P.Proxy p)
  => P.StateP [a] p () (Maybe a) y' y m ()
dropNulls = do
    ma <- Pa.draw
    case ma of
      Just a
       | I.null a  -> dropNulls
       | otherwise -> Pa.unDraw a
      Nothing      -> return ()
{-# INLINE dropNulls #-}
