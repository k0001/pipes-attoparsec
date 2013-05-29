-- | This module allows you to interleave Attoparsec parsing on input flowing
-- downstream through pipes. This module builds on top of the 'pipes-parse'
-- package, so the functionality exported by that package can be reused here.

module Control.Proxy.Attoparsec
  ( -- * Interleaved parsing
    parseD
  , maybeParseD
  , eitherParseD
    -- * Exports
  , I.ParserError(..)
  ) where

import qualified Control.Proxy                     as P
import qualified Control.Proxy.Parse               as Pa (draw, unDraw)
import qualified Control.Proxy.Attoparsec.Internal as I
import qualified Control.Proxy.Trans.Either        as Pe (EitherP, throw)
import qualified Control.Proxy.Trans.State         as Ps (StateP)
import           Data.Attoparsec.Types             (Parser)
import           Data.Foldable                     (mapM_)
import           Prelude                           hiding (mapM_)

--------------------------------------------------------------------------------
-- | Parses input flowing downstream until parsing either succeeds or fails.
--
-- In case of parsing errors, a 'ParserError' exception is thrown in the
-- 'Pe.EitherP' proxy transformer.
--
-- Requests more input from upstream using 'Pa.draw', when needed.
parseD
  :: (I.AttoparsecInput a, Monad m, P.Proxy p)
  => Parser a r
  -> ()
  -> Pe.EitherP I.ParserError (Ps.StateP [a] p) () (Maybe a) b' b m r
parseD parser = \() -> do
    (er, mlo) <- P.liftP $ I.parseWithMay Pa.draw parser
    P.liftP $ mapM_ Pa.unDraw mlo
    case er of
      Left e  -> Pe.throw e
      Right r -> return r
{-# INLINABLE parseD #-}

-- | Try to parse input flowing downstream, return 'Nothing' in case of parsing
-- failures.
--
-- Requests more input from upstream using 'Pa.draw', when needed.
maybeParseD
  :: (I.AttoparsecInput a, Monad m, P.Proxy p)
  => Parser a r
  -> ()
  -> Ps.StateP [a] p () (Maybe a) b' b m (Maybe r)
maybeParseD parser = \() -> do
    (er,mlo) <- I.parseWithMay Pa.draw parser
    mapM_ Pa.unDraw mlo
    case er of
      Left _  -> return Nothing
      Right r -> return (Just r)
{-# INLINABLE maybeParseD #-}

-- | Try to parse input flowing downstream, return 'Left' in case of parsing
-- failures.
--
-- Requests more input from upstream using 'Pa.draw', when needed.
eitherParseD
  :: (I.AttoparsecInput a, Monad m, P.Proxy p)
  => Parser a r
  -> ()
  -> Ps.StateP [a] p () (Maybe a) b' b m (Either I.ParserError r)
eitherParseD parser = \() -> do
    (er,mlo) <- I.parseWithMay Pa.draw parser
    mapM_ Pa.unDraw mlo
    return er
{-# INLINABLE eitherParseD #-}

