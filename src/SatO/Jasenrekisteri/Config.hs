module SatO.Jasenrekisteri.Config (
    Config (..),
    readConfig,
    ) where

import Database.PostgreSQL.Simple     (ConnectInfo)
import Database.PostgreSQL.Simple.URL (parseDatabaseUrl)
import System.Environment             (lookupEnv)

data Config = Config
    { cfgConnectInfo :: !ConnectInfo
    }

readConfig :: IO Config
readConfig = do
    c0 <- lookupEnv "POSTGRES_URL"
    c1 <- maybe (error "No POSTGRES_URL specified") pure c0
    c2 <- maybe (error "Invalid url") pure $ parseDatabaseUrl c1
    pure $ Config
        { cfgConnectInfo = c2
        }
