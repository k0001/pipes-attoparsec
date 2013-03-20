{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Control.Proxy.Trans.Attoparsec
  ( -- * ParseP proxy transformer
    ParseP(..)
  , runParseP
  , runParseK
    -- ** StateP support
  , get
  , put
  , modify
  , gets
    -- ** AttoparsecP proxy transformer
  , AttoparsecP
  , getLeftovers
  , popLeftovers
  , takeLeftovers
  , takeInputD
  , parseD
  ) where


import qualified Control.Exception              as Ex
import           Control.Applicative            (Applicative(..), (<$>))
import           Control.MFunctor               (MFunctor)
import           Control.Monad                  (MonadPlus)
import           Control.Monad.IO.Class         (MonadIO)
import           Control.Monad.Trans.Class      (MonadTrans)
import           Control.PFunctor               (PFunctor (..))
import           Control.Proxy                  ((>->))
import qualified Control.Proxy                  as P
import           Control.Proxy.Attoparsec.Types
import           Control.Proxy.Trans            (ProxyTrans (..))
import qualified Control.Proxy.Trans.Either     as E
import qualified Control.Proxy.Trans.State      as S
import           Data.Attoparsec.Types          (Parser)
import           Prelude                        hiding (length, null, splitAt)


newtype ParseP s p a' a b' b m r
  = ParseP { unParseP :: S.StateP s (E.EitherP Ex.SomeException p) a' a b' b m r }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadPlus,
            MonadIO, MFunctor, P.Proxy, P.MonadPlusP, P.MonadIOP)

instance ProxyTrans (ParseP s) where
  liftP = ParseP . liftP . liftP

instance PFunctor (ParseP s) where
  hoistP nat = wrap . (nat .) . unwrap
    where wrap   = ParseP . S.StateP . (E.EitherP .)
          unwrap = (E.runEitherP .) . S.unStateP . unParseP

runParseK :: s -> (t -> ParseP s p a' a b' b m r)
          -> (t -> p a' a b' b m (Either Ex.SomeException (r, s)))
runParseK s k q = runParseP s (k q)

runParseP :: s
          -> ParseP s p a' a b' b m r
          -> p a' a b' b m (Either Ex.SomeException (r, s))
runParseP s = E.runEitherP . S.runStateP s . unParseP

get :: (Monad m, P.Proxy p) => ParseP s p a' a b' b m s
get = gets id

put :: (Monad m, P.Proxy p) => s -> ParseP s p a' a b' b m ()
put s = ParseP . S.StateP $ \_ -> E.EitherP (P.return_P (Right ((),s)))

modify :: (Monad m, P.Proxy p) => (s -> s) -> ParseP s p a' a b' b m ()
modify f = ParseP . S.StateP $ \s -> E.EitherP (P.return_P (Right ((),f s)))

gets :: (Monad m, P.Proxy p) => (s -> s') -> ParseP s p a' a b' b m s'
gets f = ParseP . S.StateP $ \s -> E.EitherP (P.return_P (Right (f s,s)))


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
type AttoparsecP a = ParseP (Maybe a)

-- | Get any leftovers from the 'AttoparsecP' state.
getLeftovers
  :: (Monad m, P.Proxy p, AttoparsecInput a)
  => (AttoparsecP a p) a' a b' b m (Maybe a)
getLeftovers = get

-- | Pop any leftovers from the 'AttoparsecP' state.
popLeftovers
  :: (Monad m, P.Proxy p, AttoparsecInput a)
  => (AttoparsecP a p) a' a b' b m (Maybe a)
popLeftovers = do { s <- get; put Nothing; return s }

-- | Pop input up to length @n@ from leftovers, if any, and leave the rest.
takeLeftovers
  :: (Monad m, P.Proxy p, AttoparsecInput a)
  => Int -> (AttoparsecP a p) a' a b' b m (Maybe a)
takeLeftovers n = do
  lo <- getLeftovers
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
             Left e  -> E.throw (Ex.toException e)
             Right r -> return (r,s') ))
