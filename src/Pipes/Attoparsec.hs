{-# LANGUAGE RankNTypes #-}

-- | This module allows you to run Attoparsec parsers on input flowing
-- downstream through Pipes streams, possibly interleaving other stream
-- effects.
--
-- This Module builds on top of the @pipes-parse@ package and assumes
-- you have read "Pipes.Parse.Tutorial".

module Pipes.Attoparsec
  ( -- * Parsing
    -- $parsing
    parse
  , parseMany
  , isEndOfParserInput
    -- * Types
  , I.ParserInput(I.null)
  , I.ParsingError(..)
  ) where

--------------------------------------------------------------------------------

import           Pipes
import qualified Pipes.Parse                       as P
import qualified Pipes.Lift                        as P
import qualified Pipes.Attoparsec.Internal         as I
import qualified Control.Monad.Trans.State.Strict  as S
import           Data.Attoparsec.Types             (Parser)
import           Data.Foldable                     (mapM_)
import           Data.Function                     (fix)
import           Prelude                           hiding (mapM_)

--------------------------------------------------------------------------------

-- | Run an Attoparsec 'Parser' on input from the underlying 'Producer',
-- returning either a 'I.ParsingError' on failure, or a pair with the parsed
-- entity together with the length of input consumed in order to produce it.
parse
  :: (Monad m, I.ParserInput a)
  => Parser a b
  -> S.StateT (Producer a m r) m (Either I.ParsingError (Int, b))
parse parser = do
    (eb, mlo) <- I.parseWithMay P.draw parser
    mapM_ P.unDraw mlo
    return eb
{-# INLINABLE parse #-}

-- | Continuously run an Attoparsec 'Parser' on input from the given 'Producer',
-- sending downstream pairs of each successfully parsed entities together with
-- the length of input consumed in order to produce them.
--
-- This 'Producer' runs until it either runs out of input, in which case
-- it returns 'Nothing', or until a parsing failure occurs, in which case it
-- returns a pair of a 'I.ParsingError' and a 'Producer' with any leftovers.
parseMany
  :: (Monad m, I.ParserInput a)
  => Parser a b
  -> Producer a m r
  -> Producer (Int, b) m (Maybe (I.ParsingError, Producer a m r))
parseMany parser src = do
    r <- P.runStateP src prod
    case r of
      (Nothing, _) -> return Nothing
      (Just e,  p) -> return (Just (e, p))
  where
    prod = do
        eof <- lift isEndOfParserInput
        if eof
          then return Nothing
          else do
            eb <- lift (parse parser)
            case eb of
              Left e  -> return (Just e)
              Right b -> yield b >> prod

--------------------------------------------------------------------------------

-- | Like 'P.isEndOfInput', except it also consumes and discards leading
-- empty 'I.ParserInput' chunks.
isEndOfParserInput
  :: (I.ParserInput a, Monad m)
  => S.StateT (Producer a m r) m Bool
isEndOfParserInput = fix $ \loop -> do
    ma <- P.draw
    case ma of
      Just a
        | I.null a  -> loop
        | otherwise -> P.unDraw a >> return False
      Nothing       -> return True
{-# INLINABLE isEndOfParserInput #-}

