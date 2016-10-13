{-# LANGUAGE OverloadedStrings #-}
module SatO.Jasenrekisteri.Server (defaultMain) where

import Futurice.Prelude
import Prelude ()

import Data.Aeson.Compat
import Network.Wai
import Servant
import System.Environment (getArgs)

import qualified Data.ByteString.Lazy     as LBS
import qualified Network.Wai.Handler.Warp as Warp

import SatO.Jasenrekisteri.API
import SatO.Jasenrekisteri.Command
import SatO.Jasenrekisteri.Ctx
import SatO.Jasenrekisteri.Endpoints
import SatO.Jasenrekisteri.Markup
import SatO.Jasenrekisteri.Pages.Member
import SatO.Jasenrekisteri.Pages.Members
import SatO.Jasenrekisteri.Pages.Search
import SatO.Jasenrekisteri.Pages.Tag
import SatO.Jasenrekisteri.Pages.Tags
import SatO.Jasenrekisteri.Person
import SatO.Jasenrekisteri.Tag
import SatO.Jasenrekisteri.World

addTagEndpoint :: Ctx -> PersonId -> TagName -> Handler Text
addTagEndpoint ctx mid tn = liftIO $ do
    ctxApplyCmd (CmdAddTag mid tn) ctx
    pure "OK"

server :: Ctx -> Server JasenrekisteriAPI
server ctx = queryEndpoint ctx membersPage
    :<|> queryEndpoint ctx memberPage
    :<|> queryEndpoint ctx tagsPage
    :<|> queryEndpoint ctx tagPage
    :<|> queryEndpoint ctx searchPage
    :<|> pure (page_ "logout" (pure ()))
    :<|> addTagEndpoint ctx
    :<|> serveDirectory "static"

app :: Ctx -> Application
app = serve jasenrekisteriAPI . server

defaultMain :: IO ()
defaultMain = do
    args <- getArgs
    case args of
        [filepathData, filepathTags] -> do
            contentsData <- LBS.readFile filepathData
            persons <- decode contentsData :: IO [Person]
            -- mapM_ print $ V.filter (not . (== mempty) . _personTags) persons
            contentsTags <- LBS.readFile filepathTags
            tags <- decode contentsTags :: IO [Tag]
            let world = mkWorld persons tags
            ctx <- newCtx world
            Warp.run 8000 $ app ctx
        _ -> putStrLn "Usage: ./jasenrekisteri-server data.json tags.json"
