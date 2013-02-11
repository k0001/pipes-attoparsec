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
  , parseC
  , parseD
  ) where


import           Control.Applicative            (Applicative (..), (<$>))
import           Control.MFunctor               (MFunctor)
import           Control.Monad                  (MonadPlus)
import           Control.Monad.IO.Class         (MonadIO)
import           Control.Monad.Trans.Class      (MonadTrans)
import           Control.PFunctor               (PFunctor (..))
import           Control.Proxy                  ((>->))
import qualified Control.Proxy                  as P
import           Control.Proxy.Attoparsec.Types
import           Control.Proxy.Class            (MonadIOP, MonadPlusP, Proxy)
import           Control.Proxy.Trans            (ProxyTrans (..))
import qualified Control.Proxy.Trans.Either     as E
import qualified Control.Proxy.Trans.State      as S
import           Data.Attoparsec.Types          (Parser)
import           Prelude                        hiding (length, null, splitAt)


-- | An Either-State proxy transformer on which 'AttoparsecP' builds.
newtype ParseP e s p a' a b' b m r
  = ParseP { unParseP :: E.EitherP e (S.StateP s p) a' a b' b m r }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadTrans
    , MonadPlus
    , MonadIO
    , Proxy
    , MonadPlusP
    , MonadIOP
    , MFunctor
    )

instance ProxyTrans (ParseP e s) where
  liftP = ParseP . liftP . liftP

instance PFunctor (ParseP e s) where
  hoistP nat = wrap . (nat .) . unwrap
    where wrap   = ParseP . E.EitherP . S.StateP
          unwrap = S.unStateP . E.runEitherP . unParseP


runParseK
  :: (Proxy p, Monad m)
  => s
  -> (q -> ParseP e s p a' a b' b m r)
  -> (q -> p a' a b' b m (Either e r, s))
runParseK s k q = runParseP s $ k q

runParseP
  :: (Proxy p, Monad m)
  => s
  -> ParseP e s p a' a b' b m r
  -> p a' a b' b m (Either e r, s)
runParseP s = S.runStateP s . E.runEitherP . unParseP


get :: (Monad m, P.Proxy p) => ParseP e s p a' a b' b m s
get = gets id

put :: (Monad m, P.Proxy p) => s -> ParseP e s p a' a b' b m ()
put = ParseP . E.EitherP . fmap Right . S.put

modify :: (Monad m, P.Proxy p) => (s -> s) -> ParseP e s p a' a b' b m ()
modify = ParseP . E.EitherP . fmap Right . S.modify

gets :: (Monad m, P.Proxy p) => (s -> r) ->ParseP e s p a' a b' b m r
gets = ParseP . E.EitherP . fmap Right . S.gets


-- | Pipe input flowing downstream up to length @n@. Return any leftovers.
takeInputD'
  :: (Monad m, Proxy p, AttoparsecInput a)
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


-- | 'ParseP' specialized for Attoparsec integration.
type AttoparsecP a = ParseP ParserError (Maybe a)


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
  :: (Monad m, Proxy p, AttoparsecInput a)
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


-- | Consume and parse input from upstream until parsing succeeds or fails.
parseC
  :: (Monad m, Proxy p, AttoparsecInput a)
  => Parser a r
  -> P.Consumer (AttoparsecP a p) a m r
parseC parser = ParseP . E.EitherP . S.StateP . P.runIdentityK $ go
  where go s = parsingWith parser s $ P.request ()


-- | Parse input flowing downstream until parsing succeeds or fails.
parseD
  :: (Monad m, AttoparsecInput a, Proxy p)
  => Parser a r
  -> P.Pipe (AttoparsecP a p) a b m r
parseD parser = (const (parseC parser) >-> P.unitU) ()
