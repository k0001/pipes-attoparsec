{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Control.Proxy.Trans.Attoparsec
  ( -- * ParseP proxy transformer
    ParseP(..)
  , runParseP
  , runParseK
    -- ** AttoparsecP proxy transformer
  , AttoparsecP
  , takeLeftovers
  , takeInputD
  , parseD
  ) where

import           Control.Applicative            (Applicative(..), (<$>))
import           Control.MFunctor               (MFunctor)
import           Control.Monad                  (MonadPlus)
import           Control.Monad.IO.Class         (MonadIO)
import           Control.Monad.Trans.Class      (MonadTrans)
import           Control.Monad.State.Class      (MonadState(..))
import           Control.PFunctor               (PFunctor (..))
import           Control.Proxy                  ((>->))
import qualified Control.Proxy                  as P
import           Control.Proxy.Attoparsec.Types
import           Control.Proxy.Trans            (ProxyTrans (..))
import qualified Control.Proxy.Trans.Either     as E
import qualified Control.Proxy.Trans.State      as S
import           Data.Attoparsec.Types          (Parser)
import           Prelude                        hiding (length, null, splitAt)


newtype ParseP e s p a' a b' b m r
  = ParseP { unParseP :: S.StateP s (E.EitherP e p) a' a b' b m r }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadPlus,
            MonadIO, MFunctor, P.Proxy, P.MonadPlusP, P.MonadIOP)

instance ProxyTrans (ParseP e s) where
  liftP = ParseP . liftP . liftP

instance PFunctor (ParseP e s) where
  hoistP nat = wrap . (nat .) . unwrap
    where wrap   = ParseP . S.StateP . (E.EitherP .)
          unwrap = (E.runEitherP .) . S.unStateP . unParseP

instance (P.Proxy p, Monad m) => MonadState (ParseP e s p a' a b' b m) where
  type StateType (ParseP e s p a' a b' b m) = s
  get   = ParseP (S.StateP (\s -> E.right (s ,s)))
  put s = ParseP (S.StateP (\_ -> E.right ((),s)))

runParseK :: s -> (t -> ParseP e s p a' a b' b m r)
          -> (t -> p a' a b' b m (Either e (r, s)))
runParseK s k q = runParseP s (k q)

runParseP :: s -> ParseP e s p a' a b' b m r
          -> p a' a b' b m (Either e (r, s))
runParseP s = E.runEitherP . S.runStateP s . unParseP

-- | Pipe input flowing downstream up to length @n@. Return any leftovers.
takeInputD'
  :: (Monad m, P.Proxy p, AttoparsecInput a)
  => Int -> () -> P.Pipe p a a m (Maybe a)
takeInputD' = P.runIdentityK . go where
  go n ()
    | n <= 0    = return Nothing
    | otherwise = do
        (p,s) <- splitAt n <$> P.request ()
        P.respond p
        if null s
          then go (n - length p) ()
          else return (Just s)

-- | 'ParseP specialized for Attoparsec integration.
type AttoparsecP a = ParseP ParserError (Maybe a)

-- | Pop input up to length @n@ from leftovers, if any, and leave the rest.
takeLeftovers
  :: (Monad m, P.Proxy p, AttoparsecInput a)
  => Int -> (AttoparsecP a p) a' a b' b m (Maybe a)
takeLeftovers n = do
  lo <- get
  case fmap (splitAt n) lo of
    Nothing    -> return Nothing
    Just (p,s) -> put (mayInput s) >> return (mayInput p)

-- | Pipe input flowing downstream up to length @n@, prepending any leftovers.
takeInputD
  :: (Monad m, P.Proxy p, AttoparsecInput a)
  => Int -> () -> P.Pipe (AttoparsecP a p) a a m ()
takeInputD n ()
  | n <= 0    = return ()
  | otherwise = do
      mlo <- takeLeftovers n
      case mlo of
        Nothing -> fromUpstream n
        Just lo -> P.respond lo >> fromUpstream (n - length lo)
  where
    fromUpstream n = takeInputD' n () >>= put

-- | Parses input flowing downstream until parsing succeeds or fails.
--
-- Requests `()` upstream when more input is needed.
parseD
  :: (Monad m, AttoparsecInput a, P.Proxy p)
  => Parser a r
  -> P.Pipe (AttoparsecP a p) a x m r
parseD parser = (p >-> P.unitU) () where
  p () = ParseP (S.StateP (\s -> do
           (er,s') <- parseWith (P.request ()) parser s
           case er of
             Left e  -> E.throw e
             Right r -> return (r,s') ))
