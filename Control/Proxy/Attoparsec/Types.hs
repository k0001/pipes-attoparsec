{-# LANGUAGE DeriveDataTypeable
           , KindSignatures     #-}

module Control.Proxy.Attoparsec.Types
  ( -- * Attoparsec
    -- Integration with Attoparsec 'Parser'.
    AttoparsecInput(..)
  , ParserError(..)
    -- * Proxy types
  , ParsingProxy
  , ParsingControl
    -- ** Inter-proxy communication
  , ParsingStatus(..)
  , ParsingSupply(..)
  , supplyChunk
    -- * Error types
  , BadInput(..)
  ) where

import           Control.Proxy
import qualified Data.Attoparsec.ByteString as ABS
import qualified Data.Attoparsec.Text       as AT
import           Data.Attoparsec.Types
import qualified Data.ByteString            as BS
import qualified Data.Text                  as T
import           Data.Typeable


-- | A class for valid Attoparsec input types.
class AttoparsecInput a where
    -- | Run a 'Parser'.
    parse :: Parser a b -> a -> IResult a b
    -- | @null a@ tests whether @a@ is empty.
    null :: a -> Bool
    -- | @drop n a@ ignores the first @n@ elements from @a@ and returns the rest.
    drop :: Int -> a -> a
    -- | @length a@ returns the number of elements in @a@.
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



-- | Type synonym for a 'Proxy' which:
--
--  - Sends upstream: 'ParsingStatus' @a@
--
--  - Receives upstream: 'ParsingSupply' @a@
--
--  - Sends downstream: 'b'
--
--  - Receives downstream: @()@
type ParsingProxy (p :: * -> * -> * -> * -> (* -> *) -> * -> *) a b
  = p (ParsingStatus a) (ParsingSupply a) () b


-- | Type synonym for a 'Proxy' which:
--
--  - Sends upstream / receives downstream: 'ParsingStatus' @a@
--
--  - Receives upstream / sends downstream: 'ParsingSupply' @a@
--

type ParsingControl (p :: * -> * -> * -> * -> (* -> *) -> * -> *) a
   = p (ParsingStatus a) (ParsingSupply a) (ParsingStatus a) (ParsingSupply a)


-- | Status of a 'ParsingProxy'.
data ParsingStatus a
  -- | There is no current parsing activity.
  -- Receiving 'Resume' @a@ from upstream would feed input @a@ to a new
  -- 'Parser'.
  = Idle
  -- | There is a 'Parser' running, waiting for more @a@ input.
  -- Receiving 'Resume' @a@ from upstream would feed input @a@ to the running
  -- 'Parser'.
  | Parsing
    { psLength :: Int -- ^Length of input consumed so far.
    }
  -- | A 'Parser' has failed parsing.
  -- Receiving 'Resume' @a@ from upstream would feed input @a@ to a new
  -- 'Parser'.
  | Failed
    { psLeftover :: a -- ^Input not yet consumed when the failure occurred.
    , psError    :: ParserError -- ^Error found while parsing.
    }
  deriving (Show)


-- | A parser error as returned by Attoparsec.
data ParserError = ParserError
    { errorContexts :: [String]  -- ^ Contexts where the error occurred.
    , errorMessage  :: String    -- ^ Error message.
    } deriving (Show, Typeable)


-- | Input chunk supplied to the 'ParsingProxy'.
data ParsingSupply a
  = Start a
  -- ^ Start a new parsing activity, feed it with @a@.
  | Resume a
  -- ^ Feed @a@ to the current parsing activity, as explained by
  -- 'ParsingStatus'.
  deriving (Show)


-- | Get the input chunk from a 'ParsingSupply'.
supplyChunk :: ParsingSupply a -> a
supplyChunk (Start  x) = x
supplyChunk (Resume x) = x


-- | Reasons for a input to be unnaceptable.
data BadInput
  = InputTooLong
    { itlLenght :: Int -- ^ Length of the input.
    }
  | MalformedInput
    { miParserErrror :: ParserError -- ^ Error found while parsing.
    }
  deriving (Show)

