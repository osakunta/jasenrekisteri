module Data.ByteString.Lazy.Extra (
    readFileAscii,
    module LBS,
    ) where

import Futurice.Prelude
import Prelude ()

import Data.ByteString.Lazy    as LBS

import qualified Data.Text.Lazy.Encoding as LTE

-- | Read file ascii file, but re-encode it into utf-8.
readFileAscii :: FilePath -> IO LBS.ByteString
readFileAscii filepath = do
    contents <- LBS.readFile filepath
    pure . LTE.encodeUtf8 . LTE.decodeLatin1 $ contents
