{-# LANGUAGE OverloadedStrings #-}
module SatO.Jasenrekisteri.Config (
    Config (..),
    readConfig,
    ) where

import Database.PostgreSQL.Simple     (ConnectInfo)
import Database.PostgreSQL.Simple.URL (parseDatabaseUrl)
import Futurice.Prelude
import Prelude ()
import Servant.GoogleAuth             (GoogleClientId (..))
import System.Posix.Env.ByteString    (getEnv)
import Text.Read                      (readMaybe)

import qualified Data.ByteString.Char8 as BS8

data Config = Config
    { cfgConnectInfo  :: !ConnectInfo
    , cfgPort         :: !Int
    , cfgGcid         :: !GoogleClientId
    , cfgDataPassword :: !ByteString
    }

readConfig :: IO Config
readConfig = do
    c0 <- getEnv "POSTGRES_URL"
    c1 <- maybe (fail "No POSTGRES_URL specified") pure c0
    c2 <- maybe (fail "Invalid url") pure $ parseDatabaseUrl $ BS8.unpack c1

    p0 <- getEnv "PORT"
    p1 <- maybe (fail "No PORT specified") pure p0
    p2 <- maybe (fail "Invalid port") pure $ readMaybe $ BS8.unpack p1

    g0 <- getEnv "GOOGLE_CLIENT_ID"
    g1 <- maybe (fail "No GOOGLE_CLIENT_ID specified") pure g0
    let g2 = GoogleClientId $ decodeUtf8Lenient g1

    w0 <- getEnv "DATA_PASSWORD"
    w1 <- maybe (fail "No DATA_PASSWORD") pure w0

    pure $ Config
        { cfgConnectInfo  = c2
        , cfgPort         = p2
        , cfgGcid         = g2
        , cfgDataPassword = w1
        }
