{-# LANGUAGE DeriveDataTypeable
           , KindSignatures #-}

module Control.Proxy.Attoparsec where
-- module Control.Proxy.Attoparsec (
--   ParserError(..),
--   parserD,
--   dropBadChunks,
--   dropBadInput,
--   throwParsingErrors,
--   ) where

import           Prelude hiding (null, drop, length)
import           Control.Proxy
import           Control.Proxy.Trans.Either as PE
import qualified Data.Text as T
import qualified Data.ByteString as BS
import           Data.Attoparsec.Types
import qualified Data.Attoparsec.Text as AT
import qualified Data.Attoparsec.ByteString as ABS
import           Data.Typeable (Typeable)


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



-- | Status of the parsing proxy.
data ParserStatus a
  = Idle
  -- ^ There is no 'Parser' running.
  | Parsing Int
  -- ^ There is a 'Parser' running. The 'Int' parameter indicates the length of
  -- the input consumed so far.
  | Failed a ParserError
  -- ^ A 'Parser' has failed parsing. The @a@ parameter is the input that had
  -- not yet been consumed when the failure occurred. The 'ParseError' provides
  -- details about the failure.
  deriving (Show)


-- | Length of the input consumed so far.
pstLength :: ParserStatus a -> Int
pstLength (Parsing n) = n
pstLength _           = 0

-- | A parse error as returned by Attoparsec.
data ParserError = ParserError
    { errorContexts :: [String]  -- ^ Contexts where the error occurred.
    , errorMessage  :: String    -- ^ Error message.
    } deriving (Show, Typeable)



-- | Input chunk supplied to the parsing proxy.
data ParserSupply a
  = Start a
  -- ^ Start a new parser and feed it with @a@.
  | Continue a
  -- ^ Continue feeding the running parser with @a@.
  deriving (Show)


-- * Parsing Proxy

-- | Proxy parsing 'ParserSupply' @a@ values flowing downstream into @b@ values.
--
-- Sends 'ParserStatus' @a@ values upstream.
type ParsingProxy (p :: * -> * -> * -> * -> (* -> *) -> * -> *) a b
  = p (ParserStatus a) (ParserSupply a) () b


-- | 'ParsingProxy' parsing 'ParserSupply' @a@ values flowing 'D'ownstream into
-- @b@ values, using the given 'Parser' @a b@.
--
-- An upstream 'ParsingProxyControl' should supply new input as requested by
-- receiving a 'ParserStatus' @a@ value reporting the current status of this
-- 'ParsingProxy'.
--
-- The input given from upstream is a 'ParserSupply' @a@ which may provide more
-- input to be fed to a partial parsing result, or discard that partial result
-- and instead have the input fed to a new parsing process.
parserD :: (Proxy p, Monad m, AttoparsecInput a)
        => Parser a b
        -> ()
        -> ParsingProxy p a b m r
parserD parser () = runIdentityP . forever $ start Idle
  where
    start = req (parse parser)
    req k status = request status >>= \x -> case x of
        Start chunk    -> enough chunk >>= processNew
        Continue chunk -> enough chunk >>= process (pstLength status) k
      where enough chunk | null chunk = req k status
                         | otherwise  = return chunk
    processNew = process 0 (parse parser)
    process plen k chunk = case k chunk of
        Partial k'        -> req k' $ Parsing (plen + length chunk)
        Fail rest ctx msg -> start $ Failed rest (ParserError ctx msg)
        Done rest result  -> do
          respond result
          if null rest then start Idle
                       else processNew rest

-- * Parsing Proxy Control
--
-- TODO better explain the purpose of these


-- | Proxy supplying proper @a@ input to a 'ParsingProxy' downstream.
--
-- Recieves 'ParserStatus' @a@ from downstream.
type ParsingProxyControl (p :: * -> * -> * -> * -> (* -> *) -> * -> *) a
  = p () a (ParserStatus a) (ParserSupply a)

-- | If a downstream parsing proxy reports a parser failure, skip the whole
-- chunk being processed and keep processing new input.
--
-- Useful when an entire chunk is supposed to be well-formed, so it can be
-- safely skipped if it is not.
skipBadChunks :: (Monad m, Proxy p, AttoparsecInput a)
              => ParserStatus a
              -> p () a (ParserStatus a) (ParserSupply a) m r
skipBadChunks = runIdentityK . foreverK $ go
  where go (Parsing _)  = request () >>= respond . Continue
        go _            = request () >>= respond . Start


-- | If a downstream 'ParsingProxy' reports a parser failure, keep skipping bits
-- of input until the parser succeeds.
skipBadInput :: (Monad m, Proxy p, AttoparsecInput a)
             => ParserStatus a -> ParsingProxyControl p a m r
skipBadInput = runIdentityK . foreverK $ go
  where go (Failed rest _) = do let rest' = drop 1 rest
                                if null rest'
                                   then request () >>= respond . Continue
                                   else respond $ Continue rest'
        go (Parsing _)     = request () >>= respond . Continue
        go _               = request () >>= respond . Start

-- | If a downstream 'ParsingProxy' reports a parser failure, throw the
-- relevant 'ParserError' @a@.
throwParsingErrors
  :: (Monad m, Proxy p, AttoparsecInput a)
  => ParserStatus a
  -> ParsingProxyControl (EitherP ParserError p) a m r
throwParsingErrors = foreverK $ go
  where go (Failed _ e) = PE.throw e
        go (Parsing _)  = request () >>= respond . Continue
        go _            = request () >>= respond . Start

data InputError = InputTooLong Int
                 deriving (Show)


-- | If a downstream 'ParsingProxy' doesn't produce a value after having
-- consumed input least lenght @n@, then throw a 'InputTooLong' error.
limitInputLength
  :: (Monad m, Proxy p, AttoparsecInput a)
  => Int
  -> ParserStatus a
  -> (EitherP InputError p) (ParserStatus a) (ParserSupply a)
     (ParserStatus a) (ParserSupply a) m r
limitInputLength n = foreverK $ go
  where go (Parsing m) | m >= n = PE.throw $ InputTooLong m
        go x                    = request x >>= respond
