-- | This module provides utilities to turn your Attoparsec @'Parser' a
-- b@ into a 'Pipe' that parses 'a' values into 'b' values as they flow
-- downstream.
--
-- See "Control.Proxy.Attoparsec.Tutorial" for an extensive introduction
-- with examples.

module Control.Proxy.Attoparsec
  ( -- * Parsing Pipe
    -- $pieces
    parserInputD
  , parserD
    -- * Modules
  , module Control.Proxy.Attoparsec.Types
  , module Control.Proxy.Attoparsec.Control
  ) where

import Control.Proxy
import Control.Proxy.Attoparsec.Control
import Control.Proxy.Attoparsec.Types
import Data.Attoparsec.Types             (Parser(..), IResult(..))
import Prelude                    hiding (length, null)


-- $pieces
--
-- A 'Pipe' that parses @a@ input flowing downstream into @b@ values is
-- made of at least two smaller cooperating 'Proxy's:
--
--  1. 'parserInputD': Prepares @a@ input received from upstream to be
--     consumed by a downstream parsing 'Proxy'.
--
--  2. 'parserD': Repeatedly runs a given @'Parser' a b@ on input 'a'
--     from upstream, and sends 'b' values downstream.
--
-- Given a @'Parser' a b@ named @myParser@, the simplest way to use
-- these 'Proxy's together is:
--
-- > myParsingPipe :: (Proxy p, Monad m, AttoparsecInput a) => Pipe a b m r
-- > myParsingPipe = parserInputD >-> parserD myParser
--
-- In between these two 'Proxy's, you can place other 'Proxy's to handle
-- extraordinary situations like parsing failures or limiting input
-- length. "Control.Proxy.Attoparsec.Control" exports some useful
-- 'Proxy's that fit this place. If you skip using any of those, the
-- default behavior is that of
-- 'Control.Proxy.Attoparsec.Control.skipMalformedChunks': to simply
-- ignore all parsing errors and malformed input, and start parsing new
-- input as soon as it's available.



-- | 'Proxy' turning 'a' values flowing downstream into
-- @'ParserSupply' a@ values to be consumed by a downstream parsing
-- 'Proxy'.
--
-- This 'Proxy' responds with @'Resume' a@ to any 'ParserStatus' value
-- received from downstream.
parserInputD :: (Monad m, Proxy p)
             => ParserStatus a -> p () a (ParserStatus a) (ParserSupply a) m r
parserInputD _ = runIdentityP . forever $ request () >>= respond . Resume


-- | 'Proxy' using the given @'Parser' a b@ to repeatedly parse pieces
-- of @'ParserSupply' a@ values flowing downstream into @b@ values.
--
-- When more input is needed, a @'ParserStatus' a@ value reporting the
-- current parsing status is sent upstream, and in exchange a
-- @'ParserSupply' a@ value is expected, containing more input to be
-- parsed and directives on how to use it (see 'ParserSupply'
-- documentation).
parserD :: (Proxy p, Monad m, AttoparsecInput a)
        => Parser a b
        -> () -> p (ParserStatus a) (ParserSupply a) () b m r
parserD parser () = runIdentityP . forever $ start Idle
  where
    start = req $ parse parser
    req k status = request' status >>= \ps -> case ps of
      Start  chunk -> processNew chunk
      Resume chunk -> process (parsedLength status) k chunk
    request' status = do
      ps <- request status
      if null (supplyChunk ps) then request' status
                               else return ps
    processNew = process 0 $ parse parser
    process plen k chunk = case k chunk of
        Partial k'        -> req k' $ Parsing (plen + length chunk)
        Fail rest ctx msg -> start $ Failed rest (ParserError ctx msg)
        Done rest result  -> do
          respond result
          if null rest then start Idle
                       else processNew rest

-- | Length of input parsed so far.
parsedLength :: ParserStatus a -> Int
parsedLength (Parsing n) = n
parsedLength _           = 0




