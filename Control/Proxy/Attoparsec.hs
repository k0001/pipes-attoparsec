-- | This module provides utilities to turn your Attoparsec @'Parser' a b@ into
-- a 'Proxy' that parses 'a' values into 'b' values as they flow downstream.

module Control.Proxy.Attoparsec
  ( -- * Usage example
    -- $example-simple

    -- ** Handling errors.
    -- $example-errors

    -- * Parsing Proxy
    parserD
  , parsingPipe
  , defaultParsingPipe
    -- ** Some useful controllers
  , skipMalformedChunks
  , skipMalformedInput
  , throwParsingErrors
  , limitInputLength
  , module Control.Proxy.Attoparsec.Types
  ) where

import           Control.Proxy
import           Control.Proxy.Attoparsec.Types
import qualified Control.Proxy.Trans.Either as PE
import qualified Control.Proxy.Trans.Either as PE
import           Data.Attoparsec.Types      (Parser(..), IResult(..))
import           Prelude                    hiding (drop, length, null)



-- $example-simple
--
-- We'll write a simple 'Parser' that turns 'Text' like /“Hello John Doe.”/
-- into some @'Name' \"John Doe\"@, and then make a 'Pipe' that turns those
-- 'Text' values flowing downstream into 'Name' values flowing downstream.
--
-- In this example we are using 'Text', but we may as well use 'ByteString'.
-- Also, the 'OverloadedStrings' language extension lets us write our parser
-- easily.
--
-- > {-# LANGUAGE "OverloadedStrings" #-}
-- >
-- > import Control.Proxy
-- > import Control.Proxy.Attoparsec
-- > import Control.Proxy.Trans.Either
-- > import Data.Attoparsec.Text
-- > import Data.Text
-- >
-- > data Name = Name Text
-- >           deriving (Show)
-- >
-- > hello :: Parser Name
-- > hello = fmap Name $ "Hello" .*> skipSpace >> takeWhile1 (/='.') <*. "."
--
-- We are done with our parser, now lets make a simple 'Pipe' out of it.
--
-- > helloPipe1 :: (Proxy p, Monad m) => () -> Pipe (EitherP BadInput p) Text Name m r
-- > helloPipe1 = defaultParsingPipe hello
--
-- As the type indicates, this 'Pipe' recieves 'Text' values from upstream and
-- sends 'Name' values downstream. Through the 'EitherP' proxy transformer we
-- report upstream errors due to bad input.
--
-- We need some sample input.
--
-- > input1 :: [Text]
-- > input1 =
-- >   [ "Hello Kate."
-- >   , "Hello Mary."
-- >   ]
--
-- Now we can try our parsing pipe. We'll use @'fromListS' input1@ as our
-- input source, which sends downstream one element from the list at a time.
-- We'll call each of these elements a /chunk/. So, @'fromListS' input1@ sends
-- two /chunks/ of 'Text' downstream.
--
-- >>> runProxy . runEitherK $ fromListS input1 >-> helloPipe1 >-> printD
-- Name "Kate"
-- Name "Mary"
-- Right ()
--
-- We have acomplished our goal.


-- $example-errors
--
-- Let's try with some more complex input.
--
-- > input2 :: [Text]
-- > input2 =
-- >   [ "Hello Alice."
-- >   , "Hello Bob."
-- >   , "Hello"
-- >   , "Hello"
-- >   , "Hello World."
-- >   , "HexHello Jon."
-- >   , "H"
-- >   , "ello Ann"
-- >   , "."
-- >   , "Hello Jean-Luc."
-- >   ]
--
-- >>> runProxy . runEitherK $ fromListS input2 >-> helloPipe1 >-> printD
-- Name "Alice"
-- Name "Bob"
-- Name "HelloHello World"
-- Left (MalformedInput {miParserErrror = ParserError {errorContexts = [], errorMessage = "Failed reading: takeWith"}})
--
-- The simple @helloPipe1@ 'ParsingProxy' we built, when a parsing error is
-- arises, aborts its execution by throwing a 'BadInput' value in the 'EitherP'
-- proxy transformer. That might be enough if you are certain your input is
-- always well-formed, but sometimes you may prefer to just ignore the
-- particular input that caused the parser to fail and continue parsing new
-- input.
--
-- Handlers for two common scenarios are provided, you use them to build your
-- parsing pipe using 'parsingPipe', instead of the previously used
-- 'defaultParsingPipe'.
--
-- ['skipMalformedChunks']
--   Skips the malformed /chunk/ being parsed and requests a new chunk to be
--   parsed from start.
--
--   > helloPipe2 :: (Proxy p, Monad m) => () -> Pipe p Text Name m r
--   > helloPipe2 = parsingPipe skipMalformedChunks $ parserD hello
--
--   >>> runProxy $ fromListS input2 >-> helloPipe2 >-> printD
--   Name "Alice"
--   Name "Bob"
--   Name "HelloHello World"
--   Name "Ann"
--   Name "Jean-Luc"
--
-- ['skipMalformedInput']
--   Skips single pieces of the malformed /chunk/, one at a time, until parsing
--   succeds. It requests a new /chunk/ if needed.
--
--   > helloPipe3 :: (Proxy p, Monad m) => () -> Pipe p Text Name m r
--   > helloPipe3 = parsingPipe skipMalformedInput $ parserD hello
--
--   >>> runProxy $ fromListS input2 >-> helloPipe3 >-> printD
--   Name "Alice"
--   Name "Bob"
--   Name "HelloHello World"
--   Name "Jon"
--   Name "Ann"
--   Name "Jean-Luc"
--
--
-- Notice from those examples how we do not get Left

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



-- | If a downstream 'ParsingProxy' reports a parser failure, then throw
-- a 'MalformedInput' error.
throwParsingErrors :: (Monad m, Proxy p, AttoparsecInput a)
                   => ParsingStatus a
                   -> ParsingControl (PE.EitherP BadInput p) a m r
throwParsingErrors = foreverK $ go
  where go (Failed _ e) = PE.throw $ MalformedInput e
        go x            = request x >>= respond



-- | If a downstream 'ParsingProxy' doesn't produce a value after having
-- consumed input least lenght @n@, then throw an 'InputTooLong' error.
limitInputLength :: (Monad m, Proxy p, AttoparsecInput a)
                 => Int -> ParsingStatus a
                 -> ParsingControl (PE.EitherP BadInput p) a m r
limitInputLength n = foreverK $ go
  where go (Parsing m) | m >= n = PE.throw $ InputTooLong m
        go x                    = request x >>= respond

-- | Pipe parsing @a@ values flowing downstream into @b@ values through the
-- given 'ParsingProxy', controled by the given 'ParsingControl'.
--
-- The default control behaviour is to ignore any 'ParsingStatus' and
-- always 'Resume' parsing.
parsingPipe :: (Monad m, Proxy p, AttoparsecInput a)
            => (ParsingStatus a -> ParsingControl p a m r)
            -> (() -> ParsingProxy p a b m r)
            -> () -> Pipe p a b m r
parsingPipe control parsingp = incoming >-> control >-> parsingp
  where incoming _ = runIdentityP . forever $ request () >>= respond . Resume


-- | Pipe parsing @a@ values flowing downstream into @b@ values using the given
-- @'Parser' a b@. Throws 'MalformedInput' on parsing errors.
--
-- > defaultParsingPipe = parsingPipe throwParsingErrors . parserD
defaultParsingPipe :: (Monad m, Proxy p, AttoparsecInput a)
                   => Parser a b -> () -> Pipe (PE.EitherP BadInput p) a b m r
defaultParsingPipe = parsingPipe throwParsingErrors . parserD

