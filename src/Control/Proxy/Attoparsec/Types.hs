{-# LANGUAGE DeriveDataTypeable #-}

module Control.Proxy.Attoparsec.Types
  ( ParsingError(..)
  ) where

--------------------------------------------------------------------------------

import Data.Data         (Data, Typeable)
import Control.Exception (Exception)

--------------------------------------------------------------------------------

data ParsingError = ParsingError
  { peContexts :: [String]  -- ^ Contexts where the parsing error occurred.
  , peMessage  :: String    -- ^ Parsing error description message.
  } deriving (Show, Eq, Data, Typeable)

instance Exception ParsingError where

