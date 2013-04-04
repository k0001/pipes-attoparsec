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
import           Control.Proxy.Parse               (ParseP, drawMay, unDraw)
import qualified Control.Proxy.Attoparsec.Internal as I
import qualified Control.Proxy.Trans.Either        as E
import           Data.Attoparsec.Types             (Parser)


--------------------------------------------------------------------------------
-- | Parses input flowing downstream until it either succeeds or fails.
--
-- In case of parsing errors, a 'ParserError' exception is thrown in the
-- 'E.EitherP' proxy transformer.
--
-- Requests `()` upstream when more input is needed.
parseD :: (Monad m, I.AttoparsecInput a, P.Proxy p)
       => Parser a r
       -> P.Pipe (ParseP a (E.EitherP I.ParserError p)) (Maybe a) b m r
parseD parser = do
    (er,mlo) <- I.parseWithMay drawMay parser
    maybe (return ()) unDraw mlo
    case er of
      Left e  -> P.liftP $ E.throw e
      Right r -> return r
{-# INLINABLE parseD #-}

-- | Try to parse input flowing downstream, return 'Nothing' in case of parsing
-- failures.
--
-- Requests `()` upstream when more input is needed.
maybeParseD :: (Monad m, I.AttoparsecInput a, P.Proxy p)
            => Parser a r -> P.Pipe (ParseP a p) (Maybe a) b m (Maybe r)
maybeParseD parser = do
    (er,mlo) <- I.parseWithMay drawMay parser
    maybe (return ()) unDraw mlo
    case er of
      Left _  -> return Nothing
      Right r -> return (Just r)
{-# INLINABLE maybeParseD #-}

-- | Try to parse input flowing downstream, return 'Left' in case of Parseing
-- failures.
--
-- Requests `()` upstream when more input is needed.
eitherParseD :: (Monad m, I.AttoparsecInput a, P.Proxy p)
             => Parser a r
             -> P.Pipe (ParseP a p) (Maybe a) b m (Either I.ParserError r)
eitherParseD parser = do
    (er,mlo) <- I.parseWithMay drawMay parser
    maybe (return ()) unDraw mlo
    return er
{-# INLINABLE eitherParseD #-}

--------------------------------------------------------------------------------
-- Exported utilities
--
-- XXX: maybe we end up moving away these functions

-- | Pipe input flowing downstream up to length @n@ or first end-of-input,
-- prepending any leftovers.
--
-- Returns the input lenght, which might be less than requested if an
-- end-of-input was found.
passN :: (Monad m, P.Proxy p, I.AttoparsecInput a)
      => Int -> P.Pipe (ParseP a p) (Maybe a) a m Int
passN = onNextN P.respond
{-# INLINABLE passN #-}

-- | Drop input flowing downstream up to length @n@ or first end-of-input,
-- prepending any leftovers.
--
-- Returns the input lenght, which might be less than requested if an
-- end-of-input was found.
skipN :: (Monad m, I.AttoparsecInput a, P.Proxy p)
      => Int -> P.Pipe (ParseP a p) (Maybe a) b m Int
skipN = onNextN (const (return ()))
{-# INLINABLE skipN #-}

--------------------------------------------------------------------------------
-- Internal utilities

-- | Receive input from upstream up to length @n@ or first end-of-input,
-- prepending any previous leftovers, and apply the given action to each
-- received chunk.
--
-- Returns the input lenght, which might be less than requested if an
-- end-of-input was found.
onNextN :: (Monad m, I.AttoparsecInput a, P.Proxy p)
           => (a -> P.Pipe (ParseP  a p) (Maybe a) b m r)
           -> Int -> P.Pipe (ParseP a p) (Maybe a) b m Int
onNextN f n0 = go n0 where
  go n | n == n0   = return n
       | otherwise = do
           ma <- drawMay
           case ma of
             Nothing -> return n
             Just a  -> do
               let (p,s) = I.splitAt (n0 - n) a
               when (not (I.null s)) (unDraw s)
               f p >> go (n + I.length a)
{-# INLINABLE onNextN #-}

