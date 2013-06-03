-- | This module allows you to interleave Attoparsec parsing on input flowing
-- downstream through pipes. This module builds on top of the 'pipes-parse'
-- package, so the functionality exported by that package can be reused here.

module Control.Proxy.Attoparsec
  ( -- * Parsing
    parseD
  , parse
    -- * Utils
  , isEndOfParserInput
    -- * Types
  , I.ParserInput
  , I.ParsingError(..)
  ) where

--------------------------------------------------------------------------------

import           Control.Monad                     (unless)
import qualified Control.Proxy                     as P
import qualified Control.Proxy.Parse               as Pa
import qualified Control.Proxy.Attoparsec.Internal as I
import qualified Control.Proxy.Trans.Either        as P
import qualified Control.Proxy.Trans.State         as P
import           Data.Attoparsec.Types             (Parser)
import           Data.Foldable                     (mapM_)
import           Data.Function                     (fix)
import           Prelude                           hiding (mapM_)

--------------------------------------------------------------------------------

-- | Parses one element flowing downstream.
--
-- This proxy is meant to be composed in the 'P.request' category.
--
-- In case of parsing errors, a 'I.ParsingError' exception is thrown in
-- the 'Pe.EitherP' proxy transformer.
--
-- Requests more input from upstream using 'Pa.draw' when needed.
--
-- /Do not/ use this proxy unless 'isEndOfParserInput' returns 'False',
-- otherwise you may get unexpected boundary errors.
parse
  :: (I.ParserInput a, Monad m, P.Proxy p)
  => Parser a r
  -> () -> P.EitherP I.ParsingError (P.StateP [a] p) () (Maybe a) y' y m r
parse parser = \() -> do
    (er, mlo) <- P.liftP (I.parseWithMay Pa.draw parser)
    P.liftP (mapM_ Pa.unDraw mlo)
    either P.throw return er
{-# INLINABLE parse #-}


-- | Parses consecutive elements flowing downstream until 'isEndOfParserInput'.
--
-- This proxy is meant to be composed in the 'P.pull' category.
--
-- In case of parsing errors, a 'I.ParsingError' exception is thrown in the
-- 'Pe.EitherP' proxy transformer.
--
-- Requests more input from upstream using 'Pa.draw' when needed.
--
-- Empty input chunks flowing downstream will be discarded.
parseD
  :: (I.ParserInput a, Monad m, P.Proxy p)
  => Parser a b
  -> () -> P.Pipe (P.EitherP I.ParsingError (P.StateP [a] p)) (Maybe a) b m ()
parseD parser = \() -> loop
  where
    loop = do
        eof <- P.liftP isEndOfParserInput
        unless eof $ do
          () <- P.respond =<< parse parser ()
          loop
{-# INLINABLE parseD #-}


-- | Like 'Pa.isEndOfInput', except it also consumes and discards leading
-- empty 'I.ParserInput' chunks.
isEndOfParserInput
  :: (I.ParserInput a, Monad m, P.Proxy p)
  => P.StateP [a] p () (Maybe a) y' y m Bool
isEndOfParserInput = fix $ \loop -> do
    ma <- Pa.draw
    case ma of
      Just a
       | I.null a  -> loop
       | otherwise -> Pa.unDraw a >> return False
      Nothing      -> return True
{-# INLINABLE isEndOfParserInput #-}
