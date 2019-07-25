{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module SatO.Jasenrekisteri.Server (defaultMain) where

import Control.Concurrent.STM    (atomically, writeTVar)
import Control.Lens              (to)
import Data.List                 (foldl')
import Data.Pool                 (withResource)
import Data.Text.Encoding        (decodeUtf8)
import Futurice.IdMap            (key)
import Futurice.Prelude
import Lucid
import Network.HTTP.Types.Status (status500)

import Network.Wai        (Response, responseLBS)
import OpenSSL            (withOpenSSL)
import Prelude ()
import Servant
import Servant.GoogleAuth
import System.Entropy     (getEntropy)
import Web.Cookie

import qualified Data.ByteString.Base16   as Base16
import qualified Network.Wai.Handler.Warp as Warp

import SatO.Jasenrekisteri.API
import SatO.Jasenrekisteri.Command
import SatO.Jasenrekisteri.Config
import SatO.Jasenrekisteri.Ctx
import SatO.Jasenrekisteri.Data
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

logoutPostHandler :: Ctx -> LoginUser -> Handler Bool
logoutPostHandler ctx (LoginUser lu) = withResource (ctxPostgres ctx) $ \conn -> do
    _ <- liftIO $ P.execute conn
        "DELETE FROM jasen2.tokencache WHERE username = ?"
        (P.Only lu)
    pure True

logoutGetHandler :: Ctx -> Handler (HtmlPage "logout")
logoutGetHandler ctx = pure $ logoutPage $ ctxGcid ctx

loginHandler :: Ctx -> Handler (HtmlPage "login")
loginHandler ctx = pure $ loginPage $ ctxGcid ctx


tokensigninHandler :: Ctx -> Text -> Handler (Headers '[Header "Set-Cookie" SetCookie] LoginResponse)
tokensigninHandler ctx idtoken = do
    mgr <- liftIO $ newManager tlsManagerSettings
    eti <- liftIO $ validateToken mgr idtoken

    case eti of
        Left _err -> return $ noHeader InvalidToken
        Right ti -> withResource (ctxPostgres ctx) $ \conn -> do
            -- Check that token is for us
            if (tokenInfoAud ti /= ctxGcid ctx)
            then return $ noHeader InvalidToken
            else do
                r' <- liftIO $ P.query conn
                    "SELECT username FROM jasen2.credentials WHERE email = ?;"
                    (P.Only $ tokenInfoEmail ti)
                case r' of
                    []                    -> return $ noHeader InvalidUser
                    (P.Only username : _) -> do
                        -- delete old sessions
                        _ <- liftIO $ P.execute_ conn
                            "DELETE FROM jasen2.tokencache WHERE created < current_timestamp - '23 hours' :: interval;"

                        tokenBS <- liftIO $ Base16.encode <$> getEntropy 32
                        let tokenT = decodeUtf8 tokenBS

                        _ <- liftIO $ P.execute conn
                            "INSERT INTO jasen2.tokencache (token, username) VALUES (?, ?);"
                            (tokenT, username)

                        let setCookie :: SetCookie
                            setCookie = defaultSetCookie
                                { setCookieName   = "JASENREKISTERI_TOKEN"
                                , setCookieValue  = tokenBS
                                , setCookiePath   = Just "/"
                                , setCookieMaxAge = Just 76800
                                }
                        return $ addHeader setCookie $ LoginOK $ LoginUser username


googleAuthCheck :: Ctx -> GoogleAuthHandler LoginUser
googleAuthCheck ctx = GoogleAuthHandler handler unauthorizedErr
  where
    gcid = ctxGcid ctx

    unauthorizedErr  = err403 { errBody = renderBS $ toHtml $ loginPage gcid }

    handler :: Text -> ExceptT ServantErr IO LoginUser
    handler token = withResource (ctxPostgres ctx) $ \conn -> do
        r <- liftIO $ P.query conn
            "SELECT username FROM jasen2.tokencache WHERE token = ? AND created > current_timestamp - '1 day' :: interval;"
            (P.Only token)
        case r of
            (P.Only username : _) -> pure $ LoginUser username
            _                     -> throwError unauthorizedErr

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
    :<|> tokensigninHandler ctx
    :<|> loginHandler ctx
    :<|> logoutGetHandler ctx
    :<|> logoutPostHandler ctx
    :<|> serveDirectoryFileServer "static"

app :: Ctx -> Application
app ctx = serveWithContext jasenrekisteriAPI
    (basicAuthServerContext ctx)
    (server ctx)

exceptionResponse :: SomeException -> Response
exceptionResponse (SomeException _exc) = responseLBS status500 hdrs $ renderBS $
    doctypehtml_ $ do
        head_ $
            title_ "Error"
        body_ $ pre_ $ toHtml $ unlines
            [ "Error:"
            , replicate 72 '-'
            , "Some error" -- displayException exc
            ]
  where
    hdrs =
        [ ("Content-Type", "text/html;encoding=utf8")
        ]


defaultMain :: IO ()
defaultMain = withOpenSSL $ do
    cfg <- readConfig

    -- read initial members
    members <- getContentsData (cfgDataPassword cfg)

    -- make world
    -- mapM_ print $ V.filter (not . (== mempty) . _memberTags) members
    let world = mkWorld members tags

    -- make initial context
    ctx <- newCtx (cfgGcid cfg) (cfgConnectInfo cfg) world

    -- Query stored commands, and apply to the initial world
    cmds <- withResource (ctxPostgres ctx) $ \conn ->
        P.fromOnly <$$> P.query_ conn "SELECT edata FROM jasen2.events ORDER BY eid;"
    let world' = foldl' (flip applyCommand) world cmds
    atomically $ writeTVar (ctxWorld ctx) world'

    -- start HTTP server
    let settings :: Warp.Settings
        settings = Warp.defaultSettings
            & Warp.setPort (cfgPort cfg)
            & Warp.setOnExceptionResponse exceptionResponse
    putStrLn $ "http://localhost:" ++ show (cfgPort cfg)
    Warp.runSettings settings $ app ctx
