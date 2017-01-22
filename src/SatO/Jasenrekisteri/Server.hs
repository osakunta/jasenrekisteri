{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module SatO.Jasenrekisteri.Server (defaultMain) where

import Prelude ()
import Futurice.Prelude
import Control.Lens           (to)
import Control.Concurrent.STM (atomically, writeTVar)
import Control.Monad          (unless)
import Data.Aeson.Compat
import Data.List              (foldl')
import Data.Pool              (withResource)
import Futurice.IdMap         (key)
import Network.Wai
import Servant
import Servant.GoogleAuth
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
import SatO.Jasenrekisteri.Member
import SatO.Jasenrekisteri.Pages.Changelog
import SatO.Jasenrekisteri.Pages.Login
import SatO.Jasenrekisteri.Pages.Member
import SatO.Jasenrekisteri.Pages.Members
import SatO.Jasenrekisteri.Pages.NewMember
import SatO.Jasenrekisteri.Pages.Search
import SatO.Jasenrekisteri.Pages.Tag
import SatO.Jasenrekisteri.Pages.Tags
import SatO.Jasenrekisteri.SearchData
import SatO.Jasenrekisteri.Session
import SatO.Jasenrekisteri.Tag
import SatO.Jasenrekisteri.World

import qualified Data.UUID                  as UUID
import qualified Data.UUID.V4               as UUID
import qualified Database.PostgreSQL.Simple as P

commandEndpoint :: Ctx -> LoginUser -> Command Proxy -> Handler Text
commandEndpoint ctx lu cmd = liftIO $ do
    cmd' <- traverseCommand (\_ -> I <$> UUID.nextRandom) cmd
    ctxApplyCmd lu cmd' ctx
    pure (UUID.toText $ cmd' ^. commandMemberId)

memberlogHandler :: Ctx -> LoginUser -> MemberId -> Handler (HtmlPage "memberlog")
memberlogHandler ctx lu memberId = liftIO $ do
    cmds <- ctxFetchCmds ctx memberId
    world <- ctxReadWorld ctx
    today <- currentDay
    let gcid = ctxGcid ctx
    let origWorld = ctxOrigWorld ctx
    pure $ memberlogPage gcid today lu memberId origWorld world cmds

changelogHandler :: Ctx -> LoginUser -> Maybe CID -> Handler (HtmlPage "changelog")
changelogHandler ctx lu cid = liftIO $ do
    cmds <- ctxFetchAllCmds ctx cid
    world <- ctxReadWorld ctx
    today <- currentDay
    let gcid = ctxGcid ctx
    pure $ changelogPage gcid today lu cmds world

searchDataHandler :: Ctx -> LoginUser -> Handler [SearchItem]
searchDataHandler ctx _ = liftIO $ do
    world <- ctxReadWorld ctx
    let members = world ^.. worldMembers . folded . to memberSearchItem
    let tags' = world ^.. worldTags . ifoldedTagHierarchy . tagName . to tagSearchItem
    pure $ sort $ members ++ tags'
  where
    memberSearchItem :: Member -> SearchItem
    memberSearchItem m = SearchItem
        { searchItemLabel = m ^. memberFullName
        , searchItemValue = m ^. memberFullName
        , searchItemType  = SearchItemMember
        , searchItemHref  = memberHrefText (m ^. key)
        }

    tagSearchItem :: TagName -> SearchItem
    tagSearchItem tn@(TagName tn') = SearchItem
        { searchItemLabel = tn'
        , searchItemValue = tn'
        , searchItemType  = SearchItemTag
        , searchItemHref  = tagHrefText Nothing tn
        }

logoutHandler :: Ctx -> LoginUser -> Handler Bool
logoutHandler ctx (LoginUser lu) = withResource (ctxPostgres ctx) $ \conn -> do
    _ <- liftIO $ P.execute conn
        "DELETE FROM jasen2.tokencache WHERE username = ?"
        (P.Only lu)
    pure True

loginHandler :: Ctx -> Handler (HtmlPage "login")
loginHandler ctx = pure $ loginPage $ ctxGcid ctx

googleAuthCheck :: Ctx -> GoogleAuthHandler LoginUser
googleAuthCheck ctx = GoogleAuthHandler handler unauthorizedErr
  where
    gcid = ctxGcid ctx

    unauthorizedErr = err403 { errBody = renderBS $ toHtml $ loginPage gcid }

    handler :: Text -> ExceptT ServantErr IO LoginUser
    handler token = withResource (ctxPostgres ctx) $ \conn -> do
        r <- liftIO $ P.query conn
            "SELECT username FROM jasen2.tokencache WHERE token = ? AND created > current_timestamp - '1 day' :: interval;"
            (P.Only token)
        case r of
            (P.Only username : _) -> pure $ LoginUser username
            _                     -> do
                mgr <- liftIO $ newManager tlsManagerSettings
                ti <- liftIO $ validateToken mgr token

                -- Check it's token for us
                unless (tokenInfoAud ti == gcid) $ throwError unauthorizedErr

                r' <- liftIO $ P.query conn
                    "SELECT username FROM jasen2.credentials WHERE email = ?;"
                    (P.Only $ tokenInfoEmail ti)
                case r' of
                    []                    -> throwError unauthorizedErr
                        { errBody = "Not allowed email"
                        }
                    (P.Only username : _) -> do
                        _ <- liftIO $ P.execute_ conn
                            "DELETE FROM jasen2.tokencache WHERE created < current_timestamp - '23 hours' :: interval;"
                        _ <- liftIO $ P.execute conn
                            "INSERT INTO jasen2.tokencache (token, username) VALUES (?, ?);"
                            (token, username)
                        pure $ LoginUser username

basicAuthServerContext :: Ctx -> Context (GoogleAuthHandler LoginUser ': '[])
basicAuthServerContext ctx = googleAuthCheck ctx :. EmptyContext

server :: Ctx -> Server JasenrekisteriAPI
server ctx = queryEndpoint ctx membersPage
    :<|> queryEndpoint ctx memberPage
    :<|> queryEndpoint ctx newMemberPage
    :<|> changelogHandler ctx
    :<|> queryEndpoint ctx tagsPage
    :<|> queryEndpoint ctx tagPage
    :<|> queryEndpoint ctx searchPage
    :<|> queryEndpoint ctx searchCsv
    :<|> queryEndpoint ctx searchXlsx
    :<|> commandEndpoint ctx
    :<|> memberlogHandler ctx
    :<|> searchDataHandler ctx
    :<|> loginHandler ctx
    :<|> logoutHandler ctx
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
            members <- decode contentsData :: IO [Member]
            -- mapM_ print $ V.filter (not . (== mempty) . _memberTags) members
            let world = mkWorld members tags
            cfg <- readConfig
            let gcid = "198725857640-tl7c0h3o7mgon7h901rocnm4jfe3nlak.apps.googleusercontent.com"
            ctx <- newCtx (GoogleClientId gcid) (cfgConnectInfo cfg) world
            -- Query stored commands, and apply to the initial world
            cmds <- withResource (ctxPostgres ctx) $ \conn ->
                P.fromOnly <$$> P.query_ conn "SELECT edata FROM jasen2.events ORDER BY eid;"
            let world' = foldl' (flip applyCommand) world cmds
            atomically $ writeTVar (ctxWorld ctx) world'
            Warp.run (cfgPort cfg) $ app ctx
        _ -> putStrLn "Usage: ./jasenrekisteri-server data.json"
