{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module SatO.Jasenrekisteri.API where

import Prelude ()
import Futurice.Prelude
import Data.Csv            (EncodeOptions (..), defaultEncodeOptions)
import Lucid
import SatO.Foundation     (HtmlPage)
import Servant
import Servant.CSV.Cassava
import Servant.HTML.Lucid
import Servant.Xlsx
import Servant.GoogleAuth

import SatO.Jasenrekisteri.Command
import SatO.Jasenrekisteri.Contact
import SatO.Jasenrekisteri.Member
import SatO.Jasenrekisteri.SearchData
import SatO.Jasenrekisteri.SearchQuery
import SatO.Jasenrekisteri.Session
import SatO.Jasenrekisteri.Tag

type HTMLPageEndpoint sym = Get '[HTML] (HtmlPage sym)

-------------------------------------------------------------------------------
-- API
-------------------------------------------------------------------------------

type JasenrekisteriAuth = GoogleAuth LoginUser

type JasenrekisteriAPI =
    MembersEndpoint
    :<|> MemberEndpoint
    :<|> NewMemberEndpoint
    :<|> ChangelogEndpoint
    :<|> JasenrekisteriAuth :> "tags" :> HTMLPageEndpoint "tags"
    :<|> TagEndpoint
    :<|> SearchEndpoint
    :<|> SearchCsvEndpoint
    :<|> SearchXlsxEndpoint
    :<|> JasenrekisteriAuth :> "command" :> ReqBody '[JSON] (Command Proxy) :> Post '[JSON] Text
    :<|> MemberlogEndpoint
    :<|> JasenrekisteriAuth :> "search-data" :> Get '[JSON] [SearchItem]
    :<|> "login" :> HTMLPageEndpoint "login"
    :<|> "logout" :> JasenrekisteriAuth :> Post '[JSON] Bool
    :<|> Raw

jasenrekisteriAPI :: Proxy JasenrekisteriAPI
jasenrekisteriAPI = Proxy

-------------------------------------------------------------------------------
-- Endpoints
-------------------------------------------------------------------------------

type SearchEndpoint
    = JasenrekisteriAuth
    :> "search"
    :> QueryParam "order-by" Column
    :> QueryParam "query" SearchQuery'
    :> HTMLPageEndpoint "search"

searchEndpoint :: Proxy SearchEndpoint
searchEndpoint = Proxy

searchHrefText :: Maybe Column -> Maybe SearchQuery' -> Text
searchHrefText c q =
    linkToText $ safeLink jasenrekisteriAPI searchEndpoint c q

searchHref :: Maybe Column -> Maybe SearchQuery' -> Attribute
searchHref c q = href_ $ searchHrefText c q

type MembersEndpoint = JasenrekisteriAuth :> HTMLPageEndpoint "members"

membersEndpoint :: Proxy MembersEndpoint
membersEndpoint = Proxy

membersHrefText :: Text
membersHrefText =
    linkToText $ safeLink jasenrekisteriAPI membersEndpoint

membersHref :: Attribute
membersHref = href_ membersHrefText

type MemberEndpoint = JasenrekisteriAuth :> "member" :> Capture "id" MemberId :> HTMLPageEndpoint "member"

memberEndpoint :: Proxy MemberEndpoint
memberEndpoint = Proxy

memberHrefText :: MemberId -> Text
memberHrefText memberId =
    linkToText $ safeLink jasenrekisteriAPI memberEndpoint memberId

memberHref :: MemberId -> Attribute
memberHref = href_ . memberHrefText

type NewMemberEndpoint = JasenrekisteriAuth :> "new-member" :> HTMLPageEndpoint "new-member"

newMemberEndpoint :: Proxy NewMemberEndpoint
newMemberEndpoint = Proxy

newMemberHref :: Attribute
newMemberHref  =
    href_ $ linkToText $ safeLink jasenrekisteriAPI newMemberEndpoint

type TagEndpoint
    = JasenrekisteriAuth
    :> "tag"
    :> QueryParam "order-by" Column
    :> Capture "tag" TagName
    :> HTMLPageEndpoint "tag"

tagEndpoint :: Proxy TagEndpoint
tagEndpoint = Proxy

tagHrefText :: Maybe Column -> TagName -> Text
tagHrefText c tn =
    linkToText $ safeLink jasenrekisteriAPI tagEndpoint c tn

tagHref :: Maybe Column -> TagName -> Attribute
tagHref c tn = href_ $ tagHrefText c tn

type ChangelogEndpoint = JasenrekisteriAuth :> "changelog" :> QueryParam "eid" CID :> HTMLPageEndpoint "changelog"

changelogEndpoint :: Proxy ChangelogEndpoint
changelogEndpoint = Proxy

changelogHref :: Maybe CID -> Attribute
changelogHref cid =
    href_ $ linkToText $ safeLink jasenrekisteriAPI changelogEndpoint cid

type MemberlogEndpoint = JasenrekisteriAuth :> "member-log" :> Capture "id" MemberId :> HTMLPageEndpoint "memberlog"

memberlogEndpoint :: Proxy MemberlogEndpoint
memberlogEndpoint = Proxy

memberlogHref :: MemberId -> Attribute
memberlogHref memberId =
    href_ $ linkToText $ safeLink jasenrekisteriAPI memberlogEndpoint memberId

type SearchCsvEndpoint = JasenrekisteriAuth :> "search.csv" :> QueryParam "query" SearchQuery :> Get '[CSV' 'HasHeader SemiColonOpts] [Contact]

searchCsvEndpoint :: Proxy SearchCsvEndpoint
searchCsvEndpoint = Proxy

searchCsvHref :: SearchQuery -> Attribute
searchCsvHref query =
    href_ $ linkToText $ safeLink jasenrekisteriAPI searchCsvEndpoint (Just query)

type SearchXlsxEndpoint = JasenrekisteriAuth :> "search.xlsx" :> QueryParam "query" SearchQuery :> Get '[XLSX] SearchResult

searchXlsxEndpoint :: Proxy SearchXlsxEndpoint
searchXlsxEndpoint = Proxy

searchXlsxHref :: SearchQuery -> Attribute
searchXlsxHref query =
    href_ $ linkToText $ safeLink jasenrekisteriAPI searchXlsxEndpoint (Just query)

-------------------------------------------------------------------------------
-- Columns
-------------------------------------------------------------------------------

data Column
    = ColumnName
    | ColumnTags
    | ColumnTagsDesc
    | ColumnRoom

instance ToHttpApiData Column where
    toUrlPiece ColumnName     = "name"
    toUrlPiece ColumnTags     = "tags"
    toUrlPiece ColumnTagsDesc = "tags-desc"
    toUrlPiece ColumnRoom     = "room"

instance FromHttpApiData Column where
    parseUrlPiece "name"      = pure ColumnName
    parseUrlPiece "tags"      = pure ColumnTags
    parseUrlPiece "tags-desc" = pure ColumnTagsDesc
    parseUrlPiece "room"      = pure ColumnRoom
    parseUrlPiece _           = throwError "unknown"

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

linkToText :: Link -> Text
linkToText l = view packed $ "/" <> uriPath uri <> uriQuery uri where uri = linkURI l

-------------------------------------------------------------------------------
-- Cassava
-------------------------------------------------------------------------------

data SemiColonOpts
instance EncodeOpts SemiColonOpts where
    encodeOpts _ = defaultEncodeOptions
        { encDelimiter = fromIntegral $ fromEnum ';'
        }
