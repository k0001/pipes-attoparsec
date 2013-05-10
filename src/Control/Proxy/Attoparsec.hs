-- | This module allows you to interleave Attoparsec parsing on input flowing
-- downstream through pipes. This module builds on top of the 'pipes-parse'
-- package, so the functionality exported by that package can be reused here.

module Control.Proxy.Attoparsec
  ( -- * Interleaved parsing
    parseD
  , maybeParseD
  , eitherParseD
    -- * Utils
  , passN
  , skipN
    -- * Exports
  , I.ParserError(..)
  ) where

import           Control.Monad
import qualified Control.Proxy                     as P
import           Control.Proxy.Parse               (ParseP)
import qualified Control.Proxy.Parse               as Pa
import qualified Control.Proxy.Attoparsec.Internal as I
import qualified Control.Proxy.Trans.Either        as E
import           Data.Attoparsec.Types             (Parser)

--------------------------------------------------------------------------------
-- | Parses input flowing downstream until parsing either succeeds or fails.
--
-- In case of parsing errors, a 'ParserError' exception is thrown in the
-- 'E.EitherP' proxy transformer.
--
-- Requests `()` upstream when more input is needed.
parseD :: (I.AttoparsecInput a, P.Proxy p, Monad m)
       => Parser a r
       -> P.Pipe (ParseP a (E.EitherP I.ParserError p)) (Maybe a) b m r
parseD parser = do
    (er,mlo) <- I.parseWithMay Pa.drawMay parser
    maybe (return ()) Pa.unDraw mlo
    case er of
      Left e  -> P.liftP $ E.throw e
      Right r -> return r
{-# INLINABLE parseD #-}

-- | Try to parse input flowing downstream, return 'Nothing' in case of parsing
-- failures.
--
-- Requests `()` upstream when more input is needed.
maybeParseD :: (I.AttoparsecInput a, P.Proxy p, Monad m)
            => Parser a r -> P.Pipe (ParseP a p) (Maybe a) b m (Maybe r)
maybeParseD parser = do
    (er,mlo) <- I.parseWithMay Pa.drawMay parser
    maybe (return ()) Pa.unDraw mlo
    case er of
      Left _  -> return Nothing
      Right r -> return (Just r)
{-# INLINABLE maybeParseD #-}

-- | Try to parse input flowing downstream, return 'Left' in case of parsing
-- failures.
--
-- Requests `()` upstream when more input is needed.
eitherParseD :: (I.AttoparsecInput a, P.Proxy p, Monad m)
             => Parser a r
             -> P.Pipe (ParseP a p) (Maybe a) b m (Either I.ParserError r)
eitherParseD parser = do
    (er,mlo) <- I.parseWithMay Pa.drawMay parser
    maybe (return ()) Pa.unDraw mlo
    return er
{-# INLINABLE eitherParseD #-}

--------------------------------------------------------------------------------
-- Exported utilities
--
-- XXX: maybe we end up moving away these functions

-- | Pipe input flowing downstream up to length @n@ or first end-of-input,
-- prepending any leftovers.
--
-- Returns the passed input lenght, which might be less than requested if an
-- end-of-input was found.
passN :: (P.Proxy p, I.AttoparsecInput a, Monad m)
      => Int -> P.Pipe (ParseP a p) (Maybe a) a m Int
passN = onNextN P.respond
{-# INLINABLE passN #-}

-- | Drop input flowing downstream up to length @n@ or first end-of-input,
-- prepending any leftovers.
--
-- Returns the skipped input lenght, which might be less than requested if an
-- end-of-input was found.
skipN :: (I.AttoparsecInput a, P.Proxy p, Monad m)
      => Int -> P.Pipe (ParseP a p) (Maybe a) b m Int
skipN = onNextN (const (return ()))
{-# INLINABLE skipN #-}

--------------------------------------------------------------------------------
-- Internal utilities

-- | Receive input from upstream up to length @n@ or first end-of-input,
-- prepending any previous leftovers, and apply the given action to each
-- received chunk.
--
-- Returns the used input lenght, which might be less than requested if an
-- end-of-input was found.
onNextN :: (I.AttoparsecInput a, P.Proxy p, Monad m)
        => (a  -> P.Pipe (ParseP a p) (Maybe a) b m r)
        -> Int -> P.Pipe (ParseP a p) (Maybe a) b m Int
onNextN f n0 = go 0 where
  go n | n == n0   = return n0
       | otherwise = do
           ma <- Pa.drawMay
           case ma of
             Nothing -> return n
             Just a  -> do
               let (p,s) = I.splitAt (n0 - n) a
               when (not (I.null s)) (Pa.unDraw s)
               f p >> go (n + I.length p)
{-# INLINABLE onNextN #-}
