{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module SatO.Jasenrekisteri.Server (defaultMain) where

import Prelude ()
import Futurice.Prelude
import Control.Concurrent.STM     (atomically, readTVar, writeTVar)
import Control.Monad.CryptoRandom (crandom)
import Data.Aeson.Compat
import Data.Pool                  (withResource)
import Data.Reflection            (Given (..), give)
import Network.Wai
import Servant
import System.Environment         (getArgs)

import qualified Data.ByteString.Lazy     as LBS
import qualified Network.Wai.Handler.Warp as Warp

import SatO.Jasenrekisteri.API
import SatO.Jasenrekisteri.Command
import SatO.Jasenrekisteri.Config
import SatO.Jasenrekisteri.Ctx
import SatO.Jasenrekisteri.Endpoints
import SatO.Jasenrekisteri.Hierarchy     (tags)
import SatO.Jasenrekisteri.Markup
import SatO.Jasenrekisteri.Pages.Logout
import SatO.Jasenrekisteri.Pages.Member
import SatO.Jasenrekisteri.Pages.Members
import SatO.Jasenrekisteri.Pages.Search
import SatO.Jasenrekisteri.Pages.Tag
import SatO.Jasenrekisteri.Pages.Tags
import SatO.Jasenrekisteri.Person
import SatO.Jasenrekisteri.Session
import SatO.Jasenrekisteri.World

commandEndpoint :: Ctx -> Command -> Handler Text
commandEndpoint ctx cmd = liftIO $ do
    ctxApplyCmd cmd ctx
    pure "OK"

loginEndpoint
    :: Given (SessionStore ())
    => Ctx -> LoginData -> Handler (Maybe UUID)
loginEndpoint ctx (LoginData u p) | u == "user" && p == "pass" = do
    msid <- liftIO $ withResource (ctxPRNGs ctx) $ \tg -> atomically $ do
        g <- readTVar tg
        case crandom g of
            Left _err       -> pure Nothing
            Right (sid, g') -> writeTVar tg g' >> pure (Just sid)
    case msid of
        Nothing  -> pure Nothing
        Just sid -> do
            _ <- liftIO $ addSession ss sid ()
            pure $ Just sid
  where
    ss = given :: SessionStore ()
loginEndpoint _ _ = pure Nothing

logoutEndpoint
    :: Given (SessionStore ())
    => Ctx -> (UUID, ()) -> Handler (HtmlPage "logout")
logoutEndpoint _ctx (sid, _) = do
    liftIO $ removeSession (given :: SessionStore ()) sid
    pure logoutPage

server :: Given (SessionStore ()) => Ctx -> Server JasenrekisteriAPI
server ctx = queryEndpoint ctx membersPage
    :<|> queryEndpoint ctx memberPage
    :<|> queryEndpoint ctx tagsPage
    :<|> queryEndpoint ctx tagPage
    :<|> queryEndpoint ctx searchPage
    :<|> loginEndpoint ctx
    :<|> logoutEndpoint ctx
    :<|> commandEndpoint ctx
    :<|> serveDirectory "static"

app :: Given (SessionStore ()) => Ctx -> Application
app = serve jasenrekisteriAPI . server

defaultMain :: IO ()
defaultMain = do
    args <- getArgs
    case args of
        [filepathData, _filepathTags] -> do
            contentsData <- LBS.readFile filepathData
            persons <- decode contentsData :: IO [Person]
            -- mapM_ print $ V.filter (not . (== mempty) . _personTags) persons
            let world = mkWorld persons tags
            _cfg <- readConfig
            ctx <- newCtx world
            ss <- makeSessionStore :: IO (SessionStore ())
            Warp.run 8000 $ give ss (app ctx)
        _ -> putStrLn "Usage: ./jasenrekisteri-server data.json tags.json"
