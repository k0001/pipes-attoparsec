-- | This module provides utilities to turn your Attoparsec @'Parser' a b@ into
-- a 'Proxy' that parses 'a' values into 'b' values as they flow downstream.
--
-- See "Control.Proxy.Attoparsec.Tutorial" for an introduction.

module Control.Proxy.Attoparsec
  ( -- * Parsing Pipe construction tools
    defaultParsingPipe
  , parsingPipe
    -- * Parsing Proxy
  , parserD
    -- * Parsing Proxy Control
  , skipMalformedChunks
  , skipMalformedInput
  , throwParsingErrors
  , limitInputLength
    -- ** Util
  , noControl
    -- * Imports
  , module Control.Proxy.Attoparsec.Types
  ) where

import           Control.Proxy
import           Control.Proxy.Attoparsec.Types
import qualified Control.Proxy.Trans.Either as PE
import qualified Control.Proxy.Trans.Either as PE
import           Data.Attoparsec.Types      (Parser(..), IResult(..))
import           Prelude                    hiding (drop, length, null)


-------------------------------------------------------------------------------


-- | Pipe parsing @a@ values flowing downstream into @b@ values through the
-- given 'ParsingProxy', controled by the given 'ParsingControl'.
--
-- The default control behaviour is to ignore any 'ParsingStatus' and always
-- 'Resume' parsing.
--
-- > parsingPipe control parsingp = noControl >-> control >-> parsingp
parsingPipe :: (Monad m, Proxy p, AttoparsecInput a)
            => (ParsingStatus a -> ParsingControl p a m r)
            -> (() -> ParsingProxy p a b m r)
            -> () -> Pipe p a b m r
parsingPipe control parsingp = noControl >-> control >-> parsingp


-- | Pipe parsing @a@ values flowing downstream into @b@ values using the given
-- @'Parser' a b@. Throws 'MalformedInput' on parsing errors.
--
-- > defaultParsingPipe = parsingPipe throwParsingErrors . parserD
defaultParsingPipe :: (Monad m, Proxy p, AttoparsecInput a)
                   => Parser a b -> () -> Pipe (PE.EitherP BadInput p) a b m r
defaultParsingPipe = parsingPipe throwParsingErrors . parserD


-------------------------------------------------------------------------------


-- | 'ParsingProxy' using the given @'Parser' a b@ to parse @a@ values flowing
-- downstream into @b@ values.
--
-- When more input is needed, a @'ParsingStatus' a@ value reporting the current
-- parsing status is sent upstream, and in exchange a @'ParsingSupply' a@ value
-- is expected, containing more input to be parsed and directives on how to
-- use it (see 'ParsingSupply').
parserD :: (Proxy p, Monad m, AttoparsecInput a)
        => Parser a b
        -> ()
        -> ParsingProxy p a b m r
parserD parser () = runIdentityP . forever $ start Idle
  where
    start = req (parse parser)
    req k status = request status >>= \x -> case x of
        Start chunk    -> enough chunk >>= processNew
        Resume chunk -> enough chunk >>= process (pstLength status) k
      where enough chunk | null chunk = req k status
                         | otherwise  = return chunk
    processNew = process 0 (parse parser)
    process plen k chunk = case k chunk of
        Partial k'        -> req k' $ Parsing (plen + length chunk)
        Fail rest ctx msg -> start $ Failed rest (ParserError ctx msg)
        Done rest result  -> do
          respond result
          if null rest then start Idle
                       else processNew rest

    -- | Length of the input consumed so far.
    pstLength (Parsing n) = n
    pstLength _           = 0


-------------------------------------------------------------------------------


-- | If a downstream 'ParsingProxy' reports a parser failure, skip the whole
-- chunk being processed and start processing new input.
--
-- Useful when an entire chunk is supposed to be well-formed, so it can be
-- safely skipped if it is not.
skipMalformedChunks :: (Monad m, Proxy p, AttoparsecInput a)
                    => ParsingStatus a -> ParsingControl p a m r
skipMalformedChunks = runIdentityK . foreverK $ go
  where go (Failed _ _)  = request Idle >>= respond . Start . supplyChunk
        go x             = request x    >>= respond


-- | If a downstream 'ParsingProxy' reports a parser failure, keep skipping
-- bits of input until the parsing succeeds, and then resume normal operation.
skipMalformedInput :: (Monad m, Proxy p, AttoparsecInput a)
                   => ParsingStatus a -> ParsingControl p a m r
skipMalformedInput = runIdentityK . foreverK $ go
  where go (Failed rest _) = do
          let rest' = drop 1 rest
          if null rest'
            then request Idle >>= respond . Resume . supplyChunk
            else respond $ Resume rest'
        go x = request x >>= respond



-- | If a downstream 'ParsingProxy' reports a parser failure, then throw a
-- 'MalformedInput' error.
throwParsingErrors :: (Monad m, Proxy p, AttoparsecInput a)
                   => ParsingStatus a
                   -> ParsingControl (PE.EitherP BadInput p) a m r
throwParsingErrors = foreverK $ go
  where go (Failed _ e) = PE.throw $ MalformedInput e
        go x            = request x >>= respond



-- | If a downstream 'ParsingProxy' doesn't produce a value after having
-- consumed input of at least lenght @n@, then throw an 'InputTooLong' error in
-- the 'EitherP' proxy transformer.
limitInputLength :: (Monad m, Proxy p, AttoparsecInput a)
                 => Int -> ParsingStatus a
                 -> ParsingControl (PE.EitherP BadInput p) a m r
limitInputLength n = foreverK $ go
  where go (Parsing m) | m >= n = PE.throw $ InputTooLong m
        go x                    = request x >>= respond


-------------------------------------------------------------------------------


-- | 'Proxy' dropping any 'ParsingStatus' flowing upstream, and turning @a@
-- values flowing downstream into @'Resume' a@ values.
--
-- You may want to use this proxy immediately upstream from your first
-- 'ParsingControl' proxy.

noControl :: (Monad m, Proxy p)
          => ParsingStatus a
          -> p () a (ParsingStatus a) (ParsingSupply a) m r
noControl _ = runIdentityP . forever $ request () >>= respond . Resume
