{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE KindSignatures #-}

-- | This module exports common types used throughout
-- @"Control.Proxy.Attoparsec".*@

module Control.Proxy.Attoparsec.Types
  ( -- * Proxy control
    ParserStatus(..)
  , ParserSupply(..)
  , supplyChunk
    -- * Error types
  , BadInput(..)
    -- * Attoparsec integration
  , AttoparsecInput(..)
  , ParserError(..)
  ) where

import qualified Data.Attoparsec.ByteString as ABS
import qualified Data.Attoparsec.Text       as AT
import           Data.Attoparsec.Types
import qualified Data.ByteString            as BS
import qualified Data.Text                  as T



-- | Status of a parsing 'Proxy'.
data ParserStatus a
  -- | There is a 'Parser' running waiting for more @a@ input.
  --
  -- Receiving 'Resume' @a@ from upstream would feed input @a@ to the running
  -- 'Parser'.
  = Parsing
    { psLength :: Int -- ^Length of input consumed so far.
    }
  -- | A 'Parser' has failed parsing.
  --
  -- Receiving 'Resume' @a@ from upstream would feed input @a@ to a new
  -- 'Parser'.
  | Failed
    { psLeftover :: a -- ^Input not yet consumed when the failure occurred.
    , psError    :: ParserError -- ^Error found while parsing.
    }
  deriving (Show, Eq)


data ParserError = ParserError
    { errorContexts :: [String]  -- ^ Contexts where the error occurred.
    , errorMessage  :: String    -- ^ Error message.
    } deriving (Show, Eq)


-- | Input chunk supplied to the 'ParsingProxy'.
data ParserSupply a
  = Start a
  -- ^ Start a new parsing activity, feed it with @a@.
  | Resume a
  -- ^ Feed @a@ to the current parsing activity, as explained by
  -- 'ParserStatus'.
  deriving (Show, Eq, Functor)


-- | Get the input chunk from any 'ParserSupply' value.
supplyChunk :: ParserSupply a -> a
supplyChunk (Start  x) = x
supplyChunk (Resume x) = x


-- | Reasons for an input value to be unnaceptable.
data BadInput
  = InputTooLong
    { itlLength :: Int -- ^ Length of the input.
    }
  | MalformedInput
    { miParserErrror :: ParserError -- ^ Error found while parsing.
    }
  deriving (Show, Eq)


-- | A class for valid Attoparsec input types.
class Eq a => AttoparsecInput a where
    -- | Run a 'Parser' with input @a@.
    parse :: Parser a b -> a -> IResult a b
    -- | Tests whether @a@ is empty.
    null :: a -> Bool
    -- | Skips the first @n@ elements from @a@ and returns the rest.
    drop :: Int -> a -> a
    -- | Number of elements in @a@.
    length :: a -> Int

instance AttoparsecInput BS.ByteString where
    parse = ABS.parse
    null = BS.null
    drop = BS.drop
    length = BS.length

instance AttoparsecInput T.Text where
    parse = AT.parse
    null = T.null
    drop = T.drop
    length = T.length

