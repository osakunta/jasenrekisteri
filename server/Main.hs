{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Prelude        ()
import Prelude.Compat

import Data.Aeson.Compat
import Data.Vector        (Vector)
import Network.Wai
import Servant
import System.Environment (getArgs)

import qualified Data.ByteString.Lazy     as LBS
import qualified Data.Vector              as V
import qualified Network.Wai.Handler.Warp as Warp

import Jasenrekisteri.API
import Jasenrekisteri.HtmlUtils
import Jasenrekisteri.Pages.Member
import Jasenrekisteri.Pages.Members
import Jasenrekisteri.Pages.Tags
import Jasenrekisteri.Person

server :: Vector Person -> Server JasenrekisteriAPI
server ps = pure (template' "Jäsenrekisteri" $ pure ())
    :<|> pure (membersPage ps)
    :<|> (\i -> pure $ memberPage $ ps V.! i) -- TODO use V.!?
    :<|> pure (tagsPage $ foldMap personTags ps) -- TODO
    :<|> pure (template' "Kirjaudu ulos" $ pure ())
    :<|> serveDirectory "static"

app :: Vector Person -> Application
app = serve jasenrekisteriAPI . server

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filepath] -> do
            contents <- LBS.readFile filepath
            persons <- decode contents
            Warp.run 8000 $ app persons
        _ -> putStrLn "Usage: ./jasenrekisteri-server data.json"
