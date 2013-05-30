{-# LANGUAGE DeriveDataTypeable #-}

module Control.Proxy.Attoparsec.Types
  ( ParsingError(..)
  , ParserInput(..)
  ) where

--------------------------------------------------------------------------------

import           Data.Data                         (Data, Typeable)
import           Control.Exception                 (Exception)
import           Data.Attoparsec.Types             (Parser, IResult(..))
import qualified Data.Attoparsec.ByteString        as AB (parse)
import qualified Data.Attoparsec.Text              as AT (parse)
import qualified Data.ByteString                   as B (ByteString, null)
import           Data.Monoid                       (Monoid)
import qualified Data.Text                         as T (Text, null)
--------------------------------------------------------------------------------

data ParsingError = ParsingError
  { peContexts :: [String]  -- ^ Contexts where the parsing error occurred.
  , peMessage  :: String    -- ^ Parsing error description message.
  } deriving (Show, Eq, Data, Typeable)

instance Exception ParsingError where

--------------------------------------------------------------------------------

-- | A class for valid Attoparsec input types: 'T.Text' and 'B.ByteString'.
class (Monoid a) => ParserInput a where
    -- | Run a 'Parser' with input @a@.
    parse :: Parser a b -> a -> IResult a b
    -- | Tests whether @a@ is empty.
    null :: a -> Bool

instance ParserInput B.ByteString where
    parse   = AB.parse
    null    = B.null

instance ParserInput T.Text where
    parse   = AT.parse
    null    = T.null

