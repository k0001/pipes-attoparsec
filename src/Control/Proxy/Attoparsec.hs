-- | This module allows you to interleave Attoparsec parsing on input flowing
-- downstream through pipes. This module builds on top of the 'pipes-parse'
-- package, so the functionality exported by that package can be reused here.

module Control.Proxy.Attoparsec
  ( -- * Interleaved parsing
    parseD
  , maybeParseD
  , eitherParseD
    -- * Utils
  , takeN
  , skipN
  , passN
    -- * Exports
  , I.ParserError(..)
  ) where

import           Control.Monad
import qualified Control.Proxy                     as P
import           Control.Proxy.Parse               (ParseT)
import qualified Control.Proxy.Parse               as Pa
import qualified Control.Proxy.Attoparsec.Internal as I
import           Data.Attoparsec.Types             (Parser)

--------------------------------------------------------------------------------
-- | Parses input flowing downstream until parsing either succeeds or fails.
--
-- In case of parsing errors, a 'ParserError' exception is thrown in the
-- 'E.EitherP' proxy transformer.
--
-- Requests `()` upstream when more input is needed.
parseD :: (I.AttoparsecInput a, Monad m)
       => Parser a r -> ParseT [Maybe a] a m r
       -- -> P.Pipe (ParseP a (E.EitherP I.ParserError p)) (Maybe a) b m r
parseD parser = do
    (er,mlo) <- I.parseWithMay Pa.drawMay parser
    maybe (return ()) Pa.unDraw mlo
    case er of
      Left e  -> error "FIXME: ParseT does not use EitherP yet" -- P.liftP $ E.throw e
      Right r -> return r
{-# INLINABLE parseD #-}

-- | Try to parse input flowing downstream, return 'Nothing' in case of parsing
-- failures.
--
-- Requests `()` upstream when more input is needed.
maybeParseD :: (I.AttoparsecInput a, Monad m)
            => Parser a r -> ParseT [Maybe a] a m (Maybe r)
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
eitherParseD :: (I.AttoparsecInput a, Monad m)
             => Parser a r -> ParseT [Maybe a] a m (Either I.ParserError r)
eitherParseD parser = do
    (er,mlo) <- I.parseWithMay Pa.drawMay parser
    maybe (return ()) Pa.unDraw mlo
    return er
{-# INLINABLE eitherParseD #-}

--------------------------------------------------------------------------------
-- Exported utilities
--
-- XXX: maybe we end up moving away these functions

-- | Consume and discard input up to the specified length.
skipN :: (I.AttoparsecInput a, Monad m) => Int -> ParseT [Maybe a] a m ()
skipN = void . takeN
{-# INLINABLE skipN #-}

-- | Consume input up to the specified length and return the chunks of input
-- adding up to that lenght.
takeN :: (I.AttoparsecInput a, Monad m) => Int -> ParseT [Maybe a] a m [a]
takeN n0 = go 0 where
  go n | n == n0   = return []
       | otherwise = do
           a <- Pa.draw
           let (p,s) = I.splitAt (n0 - n) a
           when (not (I.null s)) (Pa.unDraw s)
           (p:) `liftM` go (n + I.length p)
{-# INLINABLE takeN #-}

-- | Forward input up to the specified length, prepending any given leftovers.
-- Returns any new leftovers.
passN :: (I.AttoparsecInput a, Monad m, P.Proxy p)
      => [a] -> Int -> () -> P.Pipe p a a m [a]
passN leftovers n0 () = P.runIdentityP $ go leftovers n0 where
   go lo     0 = return lo
   go []     n = P.request () >>= \a -> go [a] n
   go (a:as) n = do let (p,s) = I.splitAt (n0 - n) a
                    if I.null s
                      then P.respond p >> go as (n + I.length s)
                      else return (s:as)


