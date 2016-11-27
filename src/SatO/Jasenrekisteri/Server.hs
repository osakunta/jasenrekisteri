{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module SatO.Jasenrekisteri.Server (defaultMain) where

import Prelude ()
import Futurice.Prelude
import Control.Lens           (to)
import Control.Concurrent.STM (atomically, writeTVar)
import Data.Aeson.Compat
import Data.List              (foldl')
import Data.Pool              (withResource)
import Futurice.IdMap         (key)
import Network.Wai
import Servant
import System.Environment     (getArgs)

import qualified Data.ByteString.Lazy     as LBS
import qualified Network.Wai.Handler.Warp as Warp

import SatO.Jasenrekisteri.API
import SatO.Jasenrekisteri.Command
import SatO.Jasenrekisteri.Config
import SatO.Jasenrekisteri.Ctx
import SatO.Jasenrekisteri.Endpoints
import SatO.Jasenrekisteri.Hierarchy       (tags)
import SatO.Jasenrekisteri.Markup
import SatO.Jasenrekisteri.Pages.Changelog
import SatO.Jasenrekisteri.Pages.Member
import SatO.Jasenrekisteri.Pages.Members
import SatO.Jasenrekisteri.Pages.NewMember
import SatO.Jasenrekisteri.Pages.Search
import SatO.Jasenrekisteri.Pages.Tag
import SatO.Jasenrekisteri.Pages.Tags
import SatO.Jasenrekisteri.Person
import SatO.Jasenrekisteri.SearchData
import SatO.Jasenrekisteri.Session
import SatO.Jasenrekisteri.Tag
import SatO.Jasenrekisteri.World

import qualified Data.Text.Encoding         as TE
import qualified Data.Text.Encoding.Error   as TE
import qualified Data.UUID                  as UUID
import qualified Data.UUID.V4               as UUID
import qualified Database.PostgreSQL.Simple as P

commandEndpoint :: Ctx -> LoginUser -> Command Proxy -> Handler Text
commandEndpoint ctx lu cmd = liftIO $ do
    cmd' <- traverseCommand (\_ -> I <$> UUID.nextRandom) cmd
    ctxApplyCmd lu cmd' ctx
    pure (UUID.toText $ cmd' ^. commandMemberId)

memberlogHandler :: Ctx -> LoginUser -> PersonId -> Handler (HtmlPage "memberlog")
memberlogHandler ctx lu memberId = liftIO $ do
    cmds <- ctxFetchCmds ctx memberId
    world <- ctxReadWorld ctx
    today <- currentDay
    let origWorld = ctxOrigWorld ctx
    pure $ memberlogPage today lu memberId origWorld world cmds

changelogHandler :: Ctx -> LoginUser -> Maybe CID -> Handler (HtmlPage "changelog")
changelogHandler ctx lu cid = liftIO $ do
    cmds <- ctxFetchAllCmds ctx cid
    world <- ctxReadWorld ctx
    today <- currentDay
    pure $ changelogPage today lu cmds world

searchDataHandler :: Ctx -> LoginUser -> Handler [SearchItem]
searchDataHandler ctx _ = liftIO $ do
    world <- ctxReadWorld ctx
    let members = world ^.. worldMembers . folded . to memberSearchItem
    let tags' = world ^.. worldTags . ifoldedTagHierarchy . tagName . to tagSearchItem
    pure $ sort $ members ++ tags'
  where
    memberSearchItem :: Person -> SearchItem
    memberSearchItem m = SearchItem
        { searchItemLabel = m ^. personFullName
        , searchItemValue = m ^. personFullName
        , searchItemType  = SearchItemMember
        , searchItemHref  = memberHrefText (m ^. key)
        }

    tagSearchItem :: TagName -> SearchItem
    tagSearchItem tn@(TagName tn') = SearchItem
        { searchItemLabel = tn'
        , searchItemValue = tn'
        , searchItemType  = SearchItemTag
        , searchItemHref  = tagHrefText tn
        }


authCheck :: Ctx -> BasicAuthCheck LoginUser
authCheck ctx = BasicAuthCheck check
  where
    check (BasicAuthData username' password') =
        withResource (ctxPostgres ctx) $ \conn -> do
            let username = TE.decodeUtf8With TE.lenientDecode username'
                password = TE.decodeUtf8With TE.lenientDecode password'
            r <- P.query conn "SELECT 1 FROM jasen2.credentials where username = ? and password = ?;" (username, password)  :: IO [P.Only Int]
            pure $ case r of
                [] -> Unauthorized
                _  -> Authorized $ LoginUser username

basicAuthServerContext :: Ctx -> Context (BasicAuthCheck LoginUser ': '[])
basicAuthServerContext ctx = authCheck ctx :. EmptyContext

server :: Ctx -> Server JasenrekisteriAPI
server ctx = queryEndpoint ctx membersPage
    :<|> queryEndpoint ctx memberPage
    :<|> queryEndpoint ctx newMemberPage
    :<|> changelogHandler ctx
    :<|> queryEndpoint ctx tagsPage
    :<|> queryEndpoint ctx tagPage
    :<|> queryEndpoint ctx searchPage
    :<|> commandEndpoint ctx
    :<|> memberlogHandler ctx
    :<|> searchDataHandler ctx
    :<|> serveDirectory "static"

app :: Ctx -> Application
app ctx = serveWithContext jasenrekisteriAPI
    (basicAuthServerContext ctx)
    (server ctx)

defaultMain :: IO ()
defaultMain = do
    args <- getArgs
    case args of
        [filepathData] -> do
            contentsData <- LBS.readFile filepathData
            persons <- decode contentsData :: IO [Person]
            -- mapM_ print $ V.filter (not . (== mempty) . _personTags) persons
            let world = mkWorld persons tags
            cfg <- readConfig
            ctx <- newCtx (cfgConnectInfo cfg) world
            -- Query stored commands, and apply to the initial world
            cmds <- withResource (ctxPostgres ctx) $ \conn ->
                P.fromOnly <$$> P.query_ conn "SELECT edata FROM jasen2.events ORDER BY eid;"
            let world' = foldl' (flip applyCommand) world cmds
            atomically $ writeTVar (ctxWorld ctx) world'
            Warp.run (cfgPort cfg) $ app ctx
        _ -> putStrLn "Usage: ./jasenrekisteri-server data.json"
