{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Control.Proxy.Trans.Attoparsec
  ( ParseP(..)
  , AttoparsecP
  , runParseP
  , runParseK
  , parsePC
  ) where


import           Control.Applicative        (Applicative)
import           Control.MFunctor           (MFunctor)
import           Control.Monad.IO.Class     (MonadIO)
import           Control.Monad              (MonadPlus)
import           Control.Monad.Trans.Class  (MonadTrans)
import           Control.PFunctor           (PFunctor(..))
import           Control.Proxy.Attoparsec.Types
import           Control.Proxy.Class        (Proxy, MonadPlusP, MonadIOP)
import           Control.Proxy.Trans        (ProxyTrans(..))
import           Data.Attoparsec.Types      (Parser)
import qualified Control.Proxy              as P
import qualified Control.Proxy.Trans.Either as E
import qualified Control.Proxy.Trans.State  as S


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


type AttoparsecP a = ParseP ParserError (Maybe a)

-- | Consume and parse input from upstream until parsing succeeds or fails.
parsePC
  :: (Monad m, Proxy p, AttoparsecInput a)
  => Parser a r
  -> P.Consumer (AttoparsecP a p) a m r
parsePC parser = ParseP . E.EitherP . S.StateP . P.runIdentityK $ go
  where go s = parsingWith parser s $ P.request ()



