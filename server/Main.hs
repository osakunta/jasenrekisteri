{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Futurice.Prelude
import Prelude ()

import Data.Aeson.Compat
import Network.Wai
import Servant
import System.Environment (getArgs)

import qualified Data.ByteString.Lazy     as LBS
import qualified Network.Wai.Handler.Warp as Warp

import Jasenrekisteri.API
import Jasenrekisteri.HtmlUtils
import Jasenrekisteri.Pages.Member
import Jasenrekisteri.Pages.Members
import Jasenrekisteri.Pages.Tags
import Jasenrekisteri.Pages.Tag
import Jasenrekisteri.Person
import Jasenrekisteri.Tag
import Jasenrekisteri.World

server :: World -> Server JasenrekisteriAPI
server world = pure (membersPage world)
    :<|> (\i -> pure $ memberPage world i)
    :<|> pure (tagsPage world)
    :<|> (\i -> pure $ tagPage world i)
    :<|> pure (page_ "logout" (pure ()))
    :<|> serveDirectory "static"

app :: World-> Application
app = serve jasenrekisteriAPI . server

main :: IO ()
main = do
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
