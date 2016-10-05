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
import SatO.Jasenrekisteri.Markup
import SatO.Jasenrekisteri.Pages.Member
import SatO.Jasenrekisteri.Pages.Members
import SatO.Jasenrekisteri.Pages.Search
import SatO.Jasenrekisteri.Pages.Tags
import SatO.Jasenrekisteri.Pages.Tag
import SatO.Jasenrekisteri.Person
import SatO.Jasenrekisteri.Tag
import SatO.Jasenrekisteri.World

server :: World -> Server JasenrekisteriAPI
server world = pure (membersPage world)
    :<|> (\i -> pure $ memberPage world i)
    :<|> pure (tagsPage world)
    :<|> (\i -> pure $ tagPage world i)
    :<|> pure (searchPage world)
    :<|> pure (page_ "logout" (pure ()))
    :<|> serveDirectory "static"

app :: World-> Application
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
            Warp.run 8000 $ app world
        _ -> putStrLn "Usage: ./jasenrekisteri-server data.json tags.json"
