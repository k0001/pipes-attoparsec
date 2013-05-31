-- | This module allows you to interleave Attoparsec parsing on input flowing
-- downstream through pipes. This module builds on top of the 'pipes-parse'
-- package, so the functionality exported by that package can be reused here.

module Control.Proxy.Attoparsec
  ( -- * Parsing
    parseD
  , eitherParseD
    -- * Exports
  , module Control.Proxy.Attoparsec.Types
  ) where

--------------------------------------------------------------------------------

import qualified Control.Proxy                     as P
import qualified Control.Proxy.Parse               as Pa
import qualified Control.Proxy.Attoparsec.Internal as I
import           Control.Proxy.Attoparsec.Types
import qualified Control.Proxy.Trans.Either        as Pe (EitherP, throw)
import qualified Control.Proxy.Trans.State         as Ps (StateP)
import           Data.Attoparsec.Types             (Parser)
import           Data.Foldable                     (mapM_)
import           Prelude                           hiding (mapM_)

--------------------------------------------------------------------------------

-- | Parses input flowing downstream until parsing either succeeds or fails.
-- Returns 'Nothing' on EOF.
--
-- In case of parsing errors, a 'ParsingError' exception is thrown in the
-- 'Pe.EitherP' proxy transformer.
--
-- Requests more input from upstream using 'Pa.draw', when needed.
parseD
  :: (ParserInput a, Monad m, P.Proxy p)
  => Parser a r
  -> ()
  -> Pe.EitherP ParsingError (Ps.StateP [a] p) () (Maybe a) b' b m (Maybe r)
parseD parser = \() -> do
    eof <- P.liftP $ Pa.isEndOfInput
    if eof
      then return Nothing
      else do
        (er, mlo) <- P.liftP $ I.parseWith Pa.draw parser
        P.liftP $ mapM_ Pa.unDraw mlo
        case er of
          Left e  -> Pe.throw e
          Right r -> return (Just r)
{-# INLINABLE parseD #-}


-- | Try to parse input flowing downstream, return 'Left' in case of parsing
-- failures. Returns 'Nothing' on EOF.
--
-- Requests more input from upstream using 'Pa.draw', when needed.
eitherParseD
  :: (ParserInput a, Monad m, P.Proxy p)
  => Parser a r
  -> ()
  -> Ps.StateP [a] p () (Maybe a) b' b m (Maybe (Either ParsingError r))
eitherParseD parser = \() -> do
    eof <- Pa.isEndOfInput
    if eof
      then return Nothing
      else do
        (er,mlo) <- I.parseWith Pa.draw parser
        mapM_ Pa.unDraw mlo
        return (Just er)
{-# INLINABLE eitherParseD #-}
