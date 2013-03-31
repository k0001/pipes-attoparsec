{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Proxy.Trans.Attoparsec
  (-- * AttoparsecP proxy transformer
    AttoparsecP
  , runAttoparsecP
  , runAttoparsecK
  , parseD
  , maybeParseD
  , eitherParseD
    -- ** Utils
  , passN
  , skipN
  ) where

import           Control.Applicative            (optional, (<$>))
import           Control.Exception              (SomeException, toException)
import           Control.Monad
import           Control.Proxy                  ((>->))
import qualified Control.Proxy                  as P
import           Control.Proxy.Parse            (runParseP, drawMay, unDraw)
import           Control.Proxy.Parse.Internal   (ParseP(ParseP))
import           Control.Proxy.Attoparsec.Types
import qualified Control.Proxy.Trans.Either     as E
import qualified Control.Proxy.Trans.State      as S
import           Data.Attoparsec.Types          (Parser)
import           Data.Monoid                    (mempty, mconcat, (<>))
import           Prelude                        hiding (length, null, splitAt)


--------------------------------------------------------------------------------
-- Attoparsec interleaved parsing support for 'pipes-parse'.

-- | 'ParseP specialized for Attoparsec integration.
type AttoparsecP a p = ParseP a (E.EitherP SomeException p)

runAttoparsecP :: (Monad m, P.Proxy p)
               => AttoparsecP a p a' a b' b m r
               -> p a' a b' b m (Either SomeException r)
runAttoparsecP = E.runEitherP . runParseP

runAttoparsecK :: (Monad m, P.Proxy p)
               => (t -> AttoparsecP a p a' a b' b m r)
               -> (t -> p a' a b' b m (Either SomeException r))
runAttoparsecK k q = runAttoparsecP (k q)

--------------------------------------------------------------------------------

-- | Parses input flowing downstream until.
--
-- In case of parsing errors, a 'ParserError' exception is thrown in the
-- 'E.EitherP' proxy transformer.
--
-- Requests `()` upstream when more input is needed.
parseD :: (Monad m, AttoparsecInput a, P.Proxy p)
       => Parser a r -> P.Pipe (AttoparsecP a p) (Maybe a) b m r
parseD parser = (p >-> P.unitU) () where
  p () = ParseP (S.StateP (\s1 -> do
           -- TODO: Use 'draw'
           let s1' = mayInput . mconcat $ s1 >>= maybe [] return
           (er,s2) <- parseWith moreInput parser s1'
           case er of
             Left e  -> E.throw (toException e)
             Right r -> let s2' = maybe [] (\a -> [Just a]) s2 in
                        return (r, s2')))
  moreInput = maybe mempty id <$> P.request ()
{-# INLINABLE parseD #-}

-- | Try to parse input flowing downstream.
--
-- Requests `()` upstream when more input is needed.
maybeParseD :: (Monad m, AttoparsecInput a, P.Proxy p)
            => Parser a r -> P.Pipe (AttoparsecP a p) (Maybe a) b m (Maybe r)
maybeParseD = parseD . optional
{-# INLINABLE maybeParseD #-}

-- | Try to parse input flowing downstream.
--
-- Requests `()` upstream when more input is needed.
eitherParseD :: (Monad m, AttoparsecInput a, P.Proxy p)
             => Parser a r
             -> P.Pipe (AttoparsecP a p) (Maybe a) b m (Either ParserError r)
eitherParseD parser = (p >-> P.unitU) () where
  p () = ParseP (S.StateP (\s1 -> do
           -- TODO: Use 'draw'
           let s1' = mayInput . mconcat $ s1 >>= maybe [] return
           (er,s2) <- parseWith moreInput parser s1'
           let s2' = maybe [] (\a -> [Just a]) s2
           return (er, s2')))
  moreInput = maybe mempty id <$> P.request ()
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
passN :: (Monad m, P.Proxy p, AttoparsecInput a)
      => Int -> P.Pipe (AttoparsecP a p) (Maybe a) a m Int
passN = onNextN P.respond
{-# INLINABLE passN #-}

-- | Drop input flowing downstream up to length @n@ or first end-of-input,
-- prepending any leftovers.
--
-- Returns the input lenght, which might be less than requested if an
-- end-of-input was found.
skipN :: (Monad m, AttoparsecInput a, P.Proxy p)
      => Int -> P.Pipe (AttoparsecP a p) (Maybe a) b m Int
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
onNextN :: (Monad m, AttoparsecInput a, P.Proxy p)
           => (a -> (AttoparsecP a p) () (Maybe a) () b m r)
           -> Int -> (AttoparsecP a p) () (Maybe a) () b m Int
onNextN f n0 = go n0 where
  go n | n == n0   = return n
       | otherwise = do
           ma <- drawMay
           case ma of
             Nothing -> return n
             Just a  -> do
               let (p,s) = splitAt (n0 - n) a
               when (not (null s)) (unDraw s)
               f p >> go (n + length a)
{-# INLINABLE onNextN #-}


-- | Pop input up to length @n@ or first end-of-file from leftovers. Save any
-- leftovers.
--
-- XXX: Maybe we don't need this function, since we are not using anymore.
takeLeftovers :: (Monad m, P.Proxy p, AttoparsecInput a)
              => Int -> (AttoparsecP a p) a' a b' b m (Maybe a)
takeLeftovers n0 = ParseP (S.StateP (\s -> return (upTo n0 (Nothing, s))))
  where
    upTo _ x@(_  , [])             = x
    upTo _   (acc, (Nothing:mlos)) = (acc,mlos)
    upTo n   (acc, (Just lo:mlos))
       | n' == n   = (acc', mlos')
       | otherwise = upTo (n - n') (acc', mlos')
       where
         (p,s) = splitAt n lo
         n'    = length p
         mlos' = if null s then mlos else (Just s:mlos)
         acc'  = if null p then acc
                           else maybe (Just p) (\x -> Just (x<>p)) acc
{-# INLINABLE takeLeftovers #-}

