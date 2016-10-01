{-# LANGUAGE DeriveDataTypeable #-}
module Data.Csv.Extra (
    decodeSmart,
    module Data.Csv,
    ) where

import Futurice.Prelude
import Prelude ()

import Data.Csv

import Control.Monad.Catch (Exception, MonadThrow (..))
import Data.Char           (ord)
import Data.Typeable       (Typeable)
import Data.Vector         (Vector)

import qualified Data.ByteString.Lazy as LBS

newtype CsvDecodeException = CsvDecodeException String
    deriving (Show, Typeable)

instance Exception CsvDecodeException

decodeSmart :: (MonadThrow m, FromRecord a) => LBS.ByteString -> m (Vector a)
decodeSmart = either (throwM . CsvDecodeException) pure . decodeWith decOptions HasHeader
  where
    decOptions = defaultDecodeOptions {
        decDelimiter = fromIntegral (ord ';')
    }
