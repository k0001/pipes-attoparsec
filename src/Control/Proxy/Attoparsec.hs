-- | This module allows you to interleave Attoparsec parsing on input flowing
-- downstream through pipes. This module builds on top of the 'pipes-parse'
-- package, so the functionality exported by that package can be reused here.

module Control.Proxy.Attoparsec
  ( -- * Parsing
    parse
  , module Control.Proxy.Attoparsec.Types
  ) where

--------------------------------------------------------------------------------

import qualified Control.Proxy                     as P
import qualified Control.Proxy.Parse               as Pa
import qualified Control.Proxy.Attoparsec.Internal as I
import           Control.Proxy.Attoparsec.Types    (ParserInput, ParsingError)
import qualified Control.Proxy.Trans.Either        as Pe
import qualified Control.Proxy.Trans.State         as Ps
import           Data.Attoparsec.Types             (Parser)
import           Data.Foldable                     (mapM_)
import           Prelude                           hiding (mapM_)

--------------------------------------------------------------------------------

-- | Parses input flowing downstream until parsing either succeeds or fails.
--
-- In case of parsing errors, a 'ParsingError' exception is thrown in the
-- 'Pe.EitherP' proxy transformer.
--
-- Requests more input from upstream using 'Pa.draw', when needed.
parse
  :: (ParserInput a, Monad m, P.Proxy p)
  => Parser a r
  -> ()
  -> Pe.EitherP ParsingError (Ps.StateP [a] p) () (Maybe a) y' y m r
parse parser = \() -> do
    (er, mlo) <- P.liftP (I.parseWith Pa.draw parser)
    P.liftP (mapM_ Pa.unDraw mlo)
    either Pe.throw return er
{-# INLINABLE parse #-}

