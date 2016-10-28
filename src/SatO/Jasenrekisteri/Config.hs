module SatO.Jasenrekisteri.Config (
    Config (..),
    readConfig,
    ) where

import Database.PostgreSQL.Simple     (ConnectInfo)
import Database.PostgreSQL.Simple.URL (parseDatabaseUrl)
import System.Environment             (lookupEnv)
import Text.Read                      (readMaybe)

data Config = Config
    { cfgConnectInfo :: !ConnectInfo
    , cfgPort        :: !Int
    }

readConfig :: IO Config
readConfig = do
    c0 <- lookupEnv "POSTGRES_URL"
    c1 <- maybe (error "No POSTGRES_URL specified") pure c0
    c2 <- maybe (error "Invalid url") pure $ parseDatabaseUrl c1

    p0 <- lookupEnv "PORT"
    p1 <- maybe (error "No PORT specified") pure p0
    p2 <- maybe (error "Invalid port") pure $ readMaybe p1

    pure $ Config
        { cfgConnectInfo = c2
        , cfgPort        = p2
        }
