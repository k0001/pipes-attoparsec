-- | This module allows you to interleave Attoparsec parsing on input flowing
-- downstream through pipes. This module builds on top of the 'pipes-parse'
-- package, so the functionality exported by that package can be reused here.

module Control.Proxy.Attoparsec
  ( -- * Parsing
    parse
  , parseD
    -- * Utils
  , skipNullD
  , drawSkipNull
    -- * Types
  , module Control.Proxy.Attoparsec.Types
  ) where

--------------------------------------------------------------------------------

import           Control.Monad                     (unless)
import qualified Control.Proxy                     as P
import qualified Control.Proxy.Parse               as Pa
import qualified Control.Proxy.Attoparsec.Internal as I
import           Control.Proxy.Attoparsec.Types    (ParserInput(null),
                                                    ParsingError)
import qualified Control.Proxy.Trans.Either        as P
import qualified Control.Proxy.Trans.State         as P
import           Data.Attoparsec.Types             (Parser)
import           Data.Foldable                     (mapM_)
import           Prelude                           hiding (mapM_, null)

--------------------------------------------------------------------------------

-- | Parses one element flowing downstream.
--
-- In case of parsing errors, including EOF, a 'ParsingError' exception is
-- thrown in the 'Pe.EitherP' proxy transformer.
--
-- Requests more input from upstream using 'Pa.draw' when needed. 'null' inputs
-- from upstream will lead to premature EOF, you can prevent that kind of error
-- by using the 'skipNullD' proxy upstream.
--
-- This proxy is meant to be composed in the 'P.request' category.

-- In case you wonder, using 'I.parseWithMay' below wouldn't help if we were
-- trying to parse the tail of the stream and there are 'null' inputs left.
-- That's why we just recommend using 'skipNullD' upstream.
parse
  :: (ParserInput a, Monad m, P.Proxy p)
  => Parser a r
  -> () -> P.EitherP ParsingError (P.StateP [a] p) () (Maybe a) y' y m r
parse parser = \() -> do
    (er, mlo) <- P.liftP (I.parseWithMayNoNullCheck Pa.draw parser)
    P.liftP (mapM_ Pa.unDraw mlo)
    either P.throw return er
{-# INLINABLE parse #-}


-- | Parses elements flowing downstream until EOF.
--
-- In case of parsing errors, a 'ParsingError' exception is thrown in the
-- 'Pe.EitherP' proxy transformer.
--
-- Requests more input from upstream using 'Pa.draw', when needed. 'null' inputs
-- from upstream are discared and won't cause any parsing failures.
--
-- This proxy is meant to be composed in the 'P.pull' category.
parseD
  :: (ParserInput a, Monad m, P.Proxy p)
  => Parser a b
  -> () -> P.Pipe (P.EitherP ParsingError (P.StateP [a] p)) (Maybe a) b m ()
parseD parser = Pa.fmapPull skipNullD P.>-> \() -> loop
  where
    loop = do
        eof <- P.liftP Pa.isEndOfInput
        unless eof $ do
          () <- P.respond =<< parse parser ()
          loop
{-# INLINABLE parseD #-}


-- | Skips 'null' 'ParserInput' flowing downstream.
skipNullD :: (ParserInput a, Monad m, P.Proxy p) => () -> P.Pipe p a a m ()
skipNullD = P.filterD (not . null)
{-# INLINABLE skipNullD #-}


-- | Like 'Pa.draw', except it skips 'null' 'ParserInput'.
drawSkipNull
  :: (ParserInput a, Monad m, P.Proxy p)
  => P.StateP [a] p () (Maybe a) y' y m (Maybe a)
drawSkipNull = do
    ma <- Pa.draw
    case ma of
      Just a
       | null a    -> drawSkipNull
       | otherwise -> return ma
      Nothing      -> return Nothing
{-# INLINABLE drawSkipNull #-}

