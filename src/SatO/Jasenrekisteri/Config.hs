module SatO.Jasenrekisteri.Config (
    Config (..),
    readConfig,
    ) where

import Prelude ()
import Futurice.Prelude
import Database.PostgreSQL.Simple     (ConnectInfo)
import Database.PostgreSQL.Simple.URL (parseDatabaseUrl)
import Servant.GoogleAuth             (GoogleClientId (..))
import System.Environment             (lookupEnv)
import Text.Read                      (readMaybe)

data Config = Config
    { cfgConnectInfo :: !ConnectInfo
    , cfgPort        :: !Int
    , cfgGcid        :: !GoogleClientId
    }

readConfig :: IO Config
readConfig = do
    c0 <- lookupEnv "POSTGRES_URL"
    c1 <- maybe (error "No POSTGRES_URL specified") pure c0
    c2 <- maybe (error "Invalid url") pure $ parseDatabaseUrl c1

    p0 <- lookupEnv "PORT"
    p1 <- maybe (error "No PORT specified") pure p0
    p2 <- maybe (error "Invalid port") pure $ readMaybe p1

    g0 <- lookupEnv "GOOGLE_CLIENT_ID"
    g1 <- maybe (error "No GOOGLE_CLIENT_ID specified") pure g0
    let g2 = GoogleClientId (g1 ^. packed)

    pure $ Config
        { cfgConnectInfo = c2
        , cfgPort        = p2
        , cfgGcid        = g2
        }
