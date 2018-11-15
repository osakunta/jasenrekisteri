{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
-- |
--
-- * <https://developers.google.com/identity/sign-in/web/sign-in Sign in>
--
-- * <https://developers.google.com/identity/sign-in/web/backend-auth Backend auth>
module Servant.GoogleAuth where

import Prelude ()
import Futurice.Prelude
import Data.Aeson
       (FromJSON (..), withObject, withText, (.:), (.:?))
import Data.Aeson.Compat                          (decode)
import Network.HTTP.Client
       (Manager, httpLbs, parseRequest, responseBody)
import Network.Wai                                (requestHeaders)
import Servant
import Servant.Server.Internal                    (getContextEntry)
import Servant.Server.Internal.RoutingApplication
       (addAuthCheck, delayedFailFatal, withRequest)
import Web.Cookie                                 (parseCookiesText)

newtype GoogleClientId = GoogleClientId { getGoogleClientId :: Text }
  deriving (Eq, Show)

instance FromJSON GoogleClientId where
    parseJSON = withText "GoogleClientId" $ pure . GoogleClientId

data GoogleAuth usr

data GoogleAuthHandler usr = GoogleAuthHandler
    { unGoogleAuthHandler :: Text -> ExceptT ServantErr IO usr
    , unauthorizedError   :: !ServantErr
    }

instance HasLink api => HasLink (GoogleAuth user :> api) where
    type MkLink (GoogleAuth user :> api) a = MkLink api a
    toLink f Proxy link = toLink f (Proxy :: Proxy api) link

instance
    ( HasServer api context
    , HasContextEntry context (GoogleAuthHandler usr)
    )
    => HasServer (GoogleAuth usr :> api) context
  where
    type ServerT (GoogleAuth usr :> api) m =
        usr -> ServerT api m

    hoistServerWithContext _ pctx nt s =
        hoistServerWithContext (Proxy :: Proxy api) pctx nt . s

    route Proxy context subserver =
        route (Proxy :: Proxy api) context $ subserver `addAuthCheck` authCheck
      where
        handler :: GoogleAuthHandler usr
        handler = getContextEntry context

        authCheck = withRequest $ \req -> do
            lu <- liftIO $ runExceptT $ do
                let hdrs = requestHeaders req
                cookie <- maybe (throwError $ unauthorizedError handler ) return $ lookup "Cookie" hdrs
                token <- maybe (throwError $ unauthorizedError handler ) return $ lookup "GOOGLE_TOKEN" $ parseCookiesText cookie
                unGoogleAuthHandler handler token
            either delayedFailFatal return lu

-------------------------------------------------------------------------------
-- TokenInfo
-------------------------------------------------------------------------------

data TokenInfo = TokenInfo
    { tokenInfoEmail         :: !(Maybe Text)
    -- , tokenInfoEmailVerified :: !Bool
    , tokenInfoName          :: !Text
    , tokenInfoAud           :: !GoogleClientId
    }
  deriving
    (Eq, Show)

instance FromJSON TokenInfo where
    parseJSON = withObject "TokenInfo" $ \obj -> TokenInfo
        <$> obj .:? "email"
        -- <*> obj .: "email_verified"
        <*> obj .: "name"
        <*> obj .: "aud"

-- |
validateToken
    :: Manager
    -> Text          -- ^ auth token
    -> IO TokenInfo  -- ^ email address
validateToken mgr token = do
    req <- parseRequest $ "https://www.googleapis.com/oauth2/v3/tokeninfo?id_token=" <> token ^. unpacked
    res <- httpLbs req mgr
    decode $ responseBody res
