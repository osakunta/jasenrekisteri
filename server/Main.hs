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
import Jasenrekisteri.Context
import Jasenrekisteri.HtmlUtils
import Jasenrekisteri.Pages.Member
import Jasenrekisteri.Pages.Members
import Jasenrekisteri.Pages.Tags
import Jasenrekisteri.Tag
import Jasenrekisteri.Person

server :: JasenContext -> Server JasenrekisteriAPI
server ctx = pure (template' "Jäsenrekisteri" $ pure ())
    :<|> pure (membersPage ctx)
    :<|> (\i -> pure $ memberPage ctx i)
    :<|> pure (tagsPage ctx)
    :<|> pure (template' "Kirjaudu ulos" $ pure ())
    :<|> serveDirectory "static"

app :: JasenContext-> Application
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
            let context = mkContext persons tags
            Warp.run 8000 $ app context
        _ -> putStrLn "Usage: ./jasenrekisteri-server data.json tags.json"
