{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module SatO.Jasenrekisteri.Server (defaultMain) where

import Futurice.Prelude
import Prelude ()

import Data.Aeson.Compat
import Data.Reflection    (Given (..), give)
import Network.Wai
import Servant
import System.Environment (getArgs)

import qualified Data.ByteString.Lazy     as LBS
import qualified Network.Wai.Handler.Warp as Warp

import SatO.Jasenrekisteri.API
import SatO.Jasenrekisteri.Command
import SatO.Jasenrekisteri.Ctx
import SatO.Jasenrekisteri.Endpoints
import SatO.Jasenrekisteri.Hierarchy     (tags)
import SatO.Jasenrekisteri.Markup
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

loginEndpoint :: Given (SessionStore ()) => Ctx -> LoginData -> Handler Bool
loginEndpoint _ctx (LoginData u p) | u == "user" && p == "pass" =
    let _s = given :: SessionStore ()
    in pure True
loginEndpoint _ _ = pure False

server :: Given (SessionStore ()) => Ctx -> Server JasenrekisteriAPI
server ctx = queryEndpoint ctx membersPage
    :<|> queryEndpoint ctx memberPage
    :<|> queryEndpoint ctx tagsPage
    :<|> queryEndpoint ctx tagPage
    :<|> queryEndpoint ctx searchPage
    :<|> loginEndpoint ctx
    :<|> pure (page_ "logout" (pure ()))
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
            ctx <- newCtx world
            ss <- makeSessionStore :: IO (SessionStore ())
            Warp.run 8000 $ give ss (app ctx)
        _ -> putStrLn "Usage: ./jasenrekisteri-server data.json tags.json"
