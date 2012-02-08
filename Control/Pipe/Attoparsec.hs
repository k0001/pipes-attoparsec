{-# LANGUAGE DeriveDataTypeable #-}
module Control.Pipe.Attoparsec (
  ParseError(..),
  pipeParser,
  ) where

import Control.Exception
import Control.Pipe
import Data.Attoparsec.Types
import Data.Maybe
import Data.Monoid
import Data.Typeable

-- | A parse error as returned by Attoparsec.
data ParseError
  = ParseError {
      errorContexts :: [String],  -- ^ Contexts where the error occurred.
      errorMessage :: String      -- ^ Error message.
    }
  | DivergentParser               -- ^ Returned if a parser does not terminate
                                  -- when its input is exhausted.
    deriving (Show, Typeable)

instance Exception ParseError

-- | Convert a parser continuation into a Pipe.
--
-- To get a parser continuation from a 'Parser', use the parse function of the
-- appropriate Attoparsec module.
pipeParser :: (Monoid a, Monad m) => (a -> IResult a r) -> Pipe a x m (a, Either ParseError r)
pipeParser p = go p
  where
    go p = do
      chunk <- tryAwait
      case p (maybe mempty id chunk) of
        Fail leftover contexts msg ->
          return (leftover, Left $ ParseError contexts msg)
        Partial p' ->
          if isNothing chunk
            then return (mempty, Left DivergentParser)
            else go p'
        Done leftover result ->
          return (leftover, Right result)
