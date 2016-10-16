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
    makeSessionStore,
    -- * Login
    LoginData (..),
    -- * Servant
    Session,
    ) where

-- import Control.Lens
import Futurice.Prelude
import Prelude ()

-- import Control.Concurrent.STM
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

data SessionStore a = SessionStore

cookieName :: ByteString
cookieName = "JREK_SESSION_ID"

makeSessionStore :: IO (SessionStore a)
makeSessionStore = return SessionStore

validateSession :: SessionStore a -> ByteString -> IO (Maybe a)
validateSession _ss _bs = return Nothing -- (Just undefined)

addSession :: SessionStore a -> a -> IO ByteString
addSession _ss _x = undefined

-------------------------------------------------------------------------------
-- Servant bits
-------------------------------------------------------------------------------

data Session a

-- We could use context, but 'Given' is simpler
instance (Given (SessionStore a), HasServer api ctx)
    => HasServer (Session a :> api) ctx
  where
    type ServerT (Session a :> api) m = a -> ServerT api m

    route Proxy context subserver =
        route (Proxy :: Proxy api) context (addAuthCheck subserver sessionCheck)
      where
        ffail = delayedFailFatal err403 { errBody = page403 }
        sessionCheck = withRequest $ \req ->
            let mSessionId = do
                  h <- lookup "Cookie" (requestHeaders req)
                  lookup cookieName (parseCookies h)
            in case mSessionId of
                Nothing  -> ffail
                Just sid -> do
                    x <- liftIO $ validateSession given sid
                    maybe ffail pure x

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
