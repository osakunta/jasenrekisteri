{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module SatO.Jasenrekisteri.Data (getContentsData) where

import Data.Aeson.Compat          (decodeStrict)
import Data.ByteString.Base64     (decodeLenient)
import FileEmbedLzma              (embedByteString)
import Futurice.Prelude
import OpenSSL.EVP.Cipher
       (CryptoMode (Decrypt), cipherBS, getCipherByName)
import OpenSSL.EVP.Digest         (pkcs5_pbkdf2_hmac_sha1)
import Prelude ()
import SatO.Jasenrekisteri.Member

import qualified Data.ByteString as BS

iters :: Int
iters = 100000

encrypted :: ByteString
encrypted = $(embedByteString "encrypted/initial.json.enc")

encrypted' :: ByteString
encrypted' = decodeLenient encrypted

extract
    :: ByteString     -- ^ password
    -> ByteString     -- ^ encrypted data
    -> IO ByteString  -- ^ decrypted data
extract password bs0 = do
    when (BS.length bs0 < 16) $ fail "Too small input"

    let (magic, bs1) = BS.splitAt 8 bs0
        (salt,  enc) = BS.splitAt 8 bs1

    when (magic /= "Salted__") $ fail "No Salted__ header"
    -- BS8.putStrLn $ "salt=" <> Base16.encode salt

    let (key, iv) = BS.splitAt 32
                  $ pkcs5_pbkdf2_hmac_sha1 password salt iters 48

    -- BS8.putStrLn $ "key=" <> Base16.encode key
    -- BS8.putStrLn $ "iv= " <> Base16.encode iv

    cipher <- getCipherByName "aes-256-cbc" >>= maybe (fail "no cipher") return
    plain <- cipherBS cipher key iv Decrypt enc

    return plain

getContentsData
  :: ByteString   -- ^ password
  -> IO [Member]
getContentsData password = do
    contents <- extract password encrypted'
    decodeStrict contents
