{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Proxy.Trans.Attoparsec
  ( -- * ParseP proxy transformer
    ParseP(..)
  , unParseK
  , runParseP
  , runParseK
  , tryRunParseP
  , tryRunParseK
    -- ** AttoparsecP proxy transformer
  , AttoparsecP
  , parseD
  , maybeParseD
  , eitherParseD
    -- *** Utils
  , passN
  , skipN
  ) where

import           Control.Applicative            (Applicative(..), optional)
import           Control.Exception              (SomeException, toException)
import           Control.Monad.Morph            (MFunctor)
import           Control.Monad.IO.Class         (MonadIO)
import           Control.Monad.Trans.Class      (MonadTrans)
import           Control.Proxy                  ((>->))
import qualified Control.Proxy                  as P
import           Control.Proxy.Attoparsec.Types
import           Control.Proxy.Trans            (ProxyTrans (..))
import qualified Control.Proxy.Trans.Either     as E
import qualified Control.Proxy.Trans.State      as S
import           Data.Attoparsec.Types          (Parser)
import           Prelude                        hiding (length, null, splitAt)


newtype ParseP s p a' a b' b m r
  = ParseP { unParseP :: S.StateP s (E.EitherP SomeException p) a' a b' b m r }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO, MFunctor,
            P.Proxy, P.ProxyInternal)

unParseK :: (t -> ParseP s p a' a b' b m r)
         -> t -> S.StateP s (E.EitherP SomeException p) a' a b' b m r
unParseK = (unParseP .)

instance ProxyTrans (ParseP s) where
  liftP = ParseP . liftP . liftP

instance P.PFunctor (ParseP s) where
  hoistP nat = wrap . (nat .) . unwrap
    where wrap   = ParseP . S.StateP . (E.EitherP .)
          unwrap = (E.runEitherP .) . S.unStateP . unParseP

runParseP :: s -> ParseP s p a' a b' b m r
          -> p a' a b' b m (Either SomeException (r, s))
runParseP s = E.runEitherP . S.runStateP s . unParseP

runParseK :: s -> (t -> ParseP s p a' a b' b m r)
          -> (t -> p a' a b' b m (Either SomeException (r, s)))
runParseK s k q = runParseP s (k q)

-- | Like 'runParseP', but only only unrolls the given 'ParseP' up to an
-- 'E.EitherP SomeException' proxy transformer compatible with @ExceptionP@
-- from @pipes-safe@.
tryRunParseP :: s -> ParseP s p a' a b' b m r
             -> E.EitherP SomeException p a' a b' b m (r, s)
tryRunParseP s = S.runStateP s . unParseP

-- | Like 'runParseK', but only only unrolls the given 'ParseP' up to an
-- 'E.EitherP SomeException' proxy transformer compatible with @ExceptionP@
-- from @pipes-safe@.
tryRunParseK :: s -> (t -> ParseP s p a' a b' b m r)
             -> (t -> E.EitherP SomeException p a' a b' b m (r, s))
tryRunParseK s k q = tryRunParseP s (k q)

--------------------------------------------------------------------------------

get :: (P.Proxy p, Monad m) => ParseP s p a' a b' b m s
get = ParseP (S.StateP (\s -> E.right (s ,s)))
{-# INLINABLE get #-}

put :: (P.Proxy p, Monad m) => s -> ParseP s p a' a b' b m ()
put s = ParseP (S.StateP (\_ -> E.right ((),s)))
{-# INLINABLE put #-}

--------------------------------------------------------------------------------
-- Attoparsec interleaved parsing support

-- | 'ParseP specialized for Attoparsec integration.
type AttoparsecP a = ParseP (Maybe a)

-- | Parses input flowing downstream until.
--
-- In case of parsing errors, a 'ParserError' exception is thrown in the
-- 'E.EitherP' proxy transformer.
--
-- Requests `()` upstream when more input is needed.
parseD :: (Monad m, AttoparsecInput a, P.Proxy p)
       => Parser a r -> P.Pipe (AttoparsecP a p) a b m r
parseD parser = (p >-> P.unitU) () where
  p () = ParseP (S.StateP (\s -> do
           (er,s') <- parseWith (P.request ()) parser s
           case er of
             Left e  -> E.throw (toException e)
             Right r -> return (r,s') ))
{-# INLINABLE parseD #-}

-- | Try to parse input flowing downstream.
--
-- Requests `()` upstream when more input is needed.
maybeParseD :: (Monad m, AttoparsecInput a, P.Proxy p)
            => Parser a r -> P.Pipe (AttoparsecP a p) a b m (Maybe r)
maybeParseD = parseD . optional
{-# INLINABLE maybeParseD #-}

-- | Try to parse input flowing downstream.
--
-- Requests `()` upstream when more input is needed.
eitherParseD :: (Monad m, AttoparsecInput a, P.Proxy p)
             => Parser a r
             -> P.Pipe (AttoparsecP a p) a b m (Either ParserError r)
eitherParseD parser = (p >-> P.unitU) () where
  p () = ParseP (S.StateP (\s -> parseWith (P.request ()) parser s))
{-# INLINABLE eitherParseD #-}

--------------------------------------------------------------------------------
-- Exported utilities

-- | Pipe input flowing downstream up to length @n@, prepending any leftovers.
passN :: (Monad m, P.Proxy p, AttoparsecInput a)
      => Int -> P.Pipe (AttoparsecP a p) a a m ()
passN = nextInputN P.respond
{-# INLINABLE passN #-}

-- | Drop input flowing downstream up to length @n@, including any leftovers.
skipN :: (Monad m, AttoparsecInput a, P.Proxy p)
      => Int -> P.Pipe (AttoparsecP a p) a b m ()
skipN = nextInputN (const (return ()))
{-# INLINABLE skipN #-}

--------------------------------------------------------------------------------
-- Internal utilities

-- | Pop input up to length @n@ from leftovers. Save any leftovers.
takeLeftovers :: (Monad m, P.Proxy p, AttoparsecInput a)
              => Int -> (AttoparsecP a p) a' a b' b m (Maybe a)
takeLeftovers n = do
  lo <- get
  case fmap (splitAt n) lo of
    Nothing    -> return Nothing
    Just (p,s) -> put (mayInput s) >> return (mayInput p)
{-# INLINABLE takeLeftovers #-}

-- | Receive input from upstream up to length @n@ and apply the given action to
-- each received chunk. Return any leftovers.
reqInputN :: (Monad (p () a b' b m), Monad m, AttoparsecInput a, P.Proxy p)
          => (a -> p () a b' b m r) -> Int -> p () a b' b m (Maybe a)
reqInputN f = go where
  go n
    | n <= 0    = P.return_P Nothing
    | otherwise = do
        (p,s) <- P.request () >>= P.return_P . splitAt n
        _ <- f p
        if null s
          then go (n - length p)
          else return (Just s)
{-# INLINABLE reqInputN #-}

-- | Receive input from upstream up to length @n@ and apply the given action to
-- each received chunk, prepending any previous leftovers. Save any leftovers.
nextInputN :: (Monad m, AttoparsecInput a, P.Proxy p)
           => (a -> (AttoparsecP a p) () a b' b m r)
           -> Int -> (AttoparsecP a p) () a b' b m ()
nextInputN f n
  | n <= 0    = return ()
  | otherwise = do
      mlo <- takeLeftovers n
      case mlo of
        Nothing -> fromUpstream n
        Just lo -> f lo >> fromUpstream (n - length lo)
  where
    fromUpstream len
      | len <= 0    = return ()
      | otherwise = reqInputN f len >>= put
{-# INLINABLE nextInputN #-}
