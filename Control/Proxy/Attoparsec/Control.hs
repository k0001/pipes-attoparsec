-- | This module exports some useful 'Proxy's that act upon
-- 'ParserStatus' values received from downstream.

module Control.Proxy.Attoparsec.Control
  ( skipMalformedChunks
  , skipMalformedInput
  , throwParsingErrors
  , limitInputLength
  ) where

import           Control.Proxy
import           Control.Proxy.Attoparsec.Types
import qualified Control.Proxy.Trans.Either as PE
import           Prelude                    hiding (drop, null)

-- | If a downstream parsing 'Proxy' reports a parser failure, skip the
-- whole input /chunk/ being processed, including any left-overs, and
-- start processing new input as soon as it's available.
--
-- Useful when the input found in a single @'ParserSupply' a@ is
-- supposed to be well-formed, so it can be safely skipped if it is not.
skipMalformedChunks
  :: (Monad m, Proxy p, AttoparsecInput a)
  => ParserStatus a
  -> p (ParserStatus a) (ParserSupply a) (ParserStatus a) (ParserSupply a) m r
skipMalformedChunks = runIdentityK . foreverK $ go
  where go (Failed _ _)  = request Idle >>= respond . Start . supplyChunk
        go x             = request x    >>= respond


-- | If a downstream parsing 'Proxy' reports a parsing failure, keep
-- skipping bits of left-over input until the parsing succeeds, and then
-- resume normal parsing operation. If there are no left-overs, then
-- more input is requested from upstream.
skipMalformedInput
  :: (Monad m, Proxy p, AttoparsecInput a)
  => ParserStatus a
  -> p (ParserStatus a) (ParserSupply a) (ParserStatus a) (ParserSupply a) m r
skipMalformedInput = runIdentityK . foreverK $ go
  where go (Failed rest _) = do
          let rest' = drop 1 rest
          if null rest'
            then request Idle >>= respond . Resume . supplyChunk
            else respond $ Resume rest'
        go x = request x >>= respond


-- | If a downstream parsing 'Proxy' reports a parser failure, then
-- throw a 'MalformedInput' error.
throwParsingErrors
  :: (Monad m, Proxy p, AttoparsecInput a)
  => ParserStatus a
  -> PE.EitherP BadInput p (ParserStatus a) (ParserSupply a) (ParserStatus a) (ParserSupply a) m r
throwParsingErrors = foreverK $ go
  where go (Failed _ e) = PE.throw $ MalformedInput e
        go x            = request x >>= respond


-- | If a downstream parsing 'Proxy' doesn't produce a value after
-- having consumed input of at least lenght @n@, then throw an
-- 'InputTooLong' error in the 'EitherP' proxy transformer.
limitInputLength
  :: (Monad m, Proxy p, AttoparsecInput a)
  => Int
  -> ParserStatus a
  -> PE.EitherP BadInput p (ParserStatus a) (ParserSupply a) (ParserStatus a) (ParserSupply a) m r
limitInputLength n = foreverK $ go
  where go (Parsing m) | m >= n = PE.throw $ InputTooLong m
        go x                    = request x >>= respond

