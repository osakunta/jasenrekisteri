{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Prelude        ()
import Prelude.Compat

import Data.Aeson.Compat
--import Data.Vector        (Vector)
import Network.Wai
import Servant
import System.Environment (getArgs)

import qualified Data.ByteString.Lazy     as LBS
import qualified Data.Vector              as V
import qualified Network.Wai.Handler.Warp as Warp

import Jasenrekisteri.API
import Jasenrekisteri.Context
import Jasenrekisteri.HtmlUtils
import Jasenrekisteri.Pages.Member
import Jasenrekisteri.Pages.Members
import Jasenrekisteri.Pages.Tags
import Jasenrekisteri.Tag
import Jasenrekisteri.Types
--import Jasenrekisteri.Person

server :: JasenContext -> Server JasenrekisteriAPI
server ctx = pure (template' "Jäsenrekisteri" $ pure ())
    :<|> pure (membersPage ps)
    :<|> (\i -> pure $ memberPage ctx $ ps V.! (getUserId i)) -- TODO use V.!?
    :<|> pure (tagsPage ctx)
    :<|> pure (template' "Kirjaudu ulos" $ pure ())
    :<|> serveDirectory "static"
  where
    ps = ctxMembersDeepTags ctx

app :: JasenContext-> Application
app = serve jasenrekisteriAPI . server

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filepathData, filepathTags] -> do
            contentsData <- LBS.readFile filepathData
            persons <- decode contentsData
            contentsTags <- LBS.readFile filepathTags
            tags <- decode contentsTags
            let context = mkContext persons (tagHierarchy tags) (tagColours tags)
            Warp.run 8000 $ app context
        _ -> putStrLn "Usage: ./jasenrekisteri-server data.json tags.json"
