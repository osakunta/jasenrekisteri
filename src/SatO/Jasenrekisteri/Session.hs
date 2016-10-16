{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module SatO.Jasenrekisteri.Session (
    SessionStore,
    addSession,
    removeSession,
    makeSessionStore,
    -- * Login
    LoginData (..),
    -- * Servant
    Session,
    ) where

import Prelude ()
import Futurice.Prelude
import Control.Concurrent.STM
       (TVar, atomically, modifyTVar', newTVarIO, readTVarIO)
import Data.Aeson.Compat
import Data.ByteString                            (ByteString)
import Data.Reflection                            (Given (..))
import Lucid
import Network.Wai                                (requestHeaders)
import SatO.Foundation
import Servant
import Servant.Server.Internal.RoutingApplication
       (addAuthCheck, delayedFailFatal, withRequest)
import Servant.Server.Internal.ServantErr         (err403)
import Web.Cookie                                 (parseCookies)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.UUID            as UUID

data SessionStore a = SessionStore (TVar (Map UUID a))

cookieName :: ByteString
cookieName = "JREK_SESSION_ID"

makeSessionStore :: IO (SessionStore a)
makeSessionStore = SessionStore <$> newTVarIO mempty

validateSession :: SessionStore a -> UUID -> IO (Maybe a)
validateSession (SessionStore ss) sid = do
    m <- readTVarIO ss
    return $ m ^. at sid

addSession :: SessionStore a -> UUID -> a -> IO ()
addSession (SessionStore ss) sid x = atomically $ modifyTVar' ss $ \m ->
    m & at sid ?~ x

removeSession :: SessionStore a -> UUID -> IO ()
removeSession  (SessionStore ss) sid = atomically $ modifyTVar' ss $ \m ->
    m & at sid .~ Nothing

-------------------------------------------------------------------------------
-- Servant bits
-------------------------------------------------------------------------------

data Session a

-- We could use context, but 'Given' is simpler
instance (Given (SessionStore a), HasServer api ctx)
    => HasServer (Session a :> api) ctx
  where
    type ServerT (Session a :> api) m = (UUID, a) -> ServerT api m

    route Proxy context subserver =
        route (Proxy :: Proxy api) context (addAuthCheck subserver sessionCheck)
      where
        ffail = delayedFailFatal err403 { errBody = page403 }
        sessionCheck = withRequest $ \req ->
            let mSessionId = do
                  h <- lookup "Cookie" (requestHeaders req)
                  bs <- lookup cookieName (parseCookies h)
                  UUID.fromASCIIBytes bs
            in case mSessionId of
                Nothing  -> ffail
                Just sid -> do
                    x <- liftIO $ validateSession given sid
                    maybe ffail (pure . (,) sid) x

instance HasLink api => HasLink (Session a :> api) where
    type MkLink (Session a :> api) = MkLink api
    toLink Proxy = toLink (Proxy :: Proxy api)

-------------------------------------------------------------------------------
-- LoginData
-------------------------------------------------------------------------------

data LoginData = LoginData
    { loginUser :: !Text
    , loginPass :: !Text
    }

instance FromJSON LoginData where
    parseJSON = withObject "LoginData" $ \obj -> LoginData
        <$> obj .: "user"
        <*> obj .: "pass"

-------------------------------------------------------------------------------
-- error page
-------------------------------------------------------------------------------

page403 :: LBS.ByteString
page403 = renderBS $ toHtml $ page_ "Jäsenrekisteri" $ do
    row_ $ large_ 12 $ h1_ "Kirjaudu"
    row_ $ large_ 12 $ div_ [ id_ "login-form", class_ "callout primary" ] $ do
        row_ $ large_ 12 $ label_ $ do
            "Tunnus"
            input_ [ id_ "login-user" , type_ "text" ]
        row_ $ large_ 12 $ label_ $ do
            "Tunnus"
            input_ [ id_ "login-pass" , type_ "password" ]
        row_ $ large_ 12 $ button_ [ id_ "login-button", class_ "button "] $
            "Kirjaudu"
