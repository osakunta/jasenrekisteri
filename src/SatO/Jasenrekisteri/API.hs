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

type JasenrekisteriAuth = BasicAuth "jasenrekisteri" LoginUser

type JasenrekisteriAPI =
    JasenrekisteriAuth :> HTMLPageEndpoint "members"
    :<|> MemberEndpoint
    :<|> NewMemberEndpoint
    :<|> ChangelogEndpoint
    :<|> JasenrekisteriAuth :> "tags" :> HTMLPageEndpoint "tags"
    :<|> TagEndpoint
    :<|> JasenrekisteriAuth :> "search" :> QueryParam "query" SearchQuery' :> HTMLPageEndpoint "search"
    :<|> SearchCsvEndpoint
    :<|> SearchXlsxEndpoint
    :<|> JasenrekisteriAuth :> "command" :> ReqBody '[JSON] (Command Proxy) :> Post '[JSON] Text
    :<|> MemberlogEndpoint
    :<|> JasenrekisteriAuth :> "search-data" :> Get '[JSON] [SearchItem]
    :<|> Raw

jasenrekisteriAPI :: Proxy JasenrekisteriAPI
jasenrekisteriAPI = Proxy

-------------------------------------------------------------------------------
-- Endpoints
-------------------------------------------------------------------------------

type MemberEndpoint = JasenrekisteriAuth :> "member" :> Capture "id" MemberId :> HTMLPageEndpoint "member"

memberEndpoint :: Proxy MemberEndpoint
memberEndpoint = Proxy

memberHrefText :: MemberId -> Text
memberHrefText memberId =
    uriToText $ safeLink jasenrekisteriAPI memberEndpoint memberId

memberHref :: MemberId -> Attribute
memberHref = href_ . memberHrefText

type NewMemberEndpoint = JasenrekisteriAuth :> "new-member" :> HTMLPageEndpoint "new-member"

newMemberEndpoint :: Proxy NewMemberEndpoint
newMemberEndpoint = Proxy

newMemberHref :: Attribute
newMemberHref  =
    href_ $ uriToText $ safeLink jasenrekisteriAPI newMemberEndpoint

type TagEndpoint = JasenrekisteriAuth :> "tag" :> Capture "tag" TagName :> HTMLPageEndpoint "tag"

tagEndpoint :: Proxy TagEndpoint
tagEndpoint = Proxy

tagHrefText :: TagName -> Text
tagHrefText tn =
    uriToText $ safeLink jasenrekisteriAPI tagEndpoint tn

tagHref :: TagName -> Attribute
tagHref = href_ . tagHrefText

type ChangelogEndpoint = JasenrekisteriAuth :> "changelog" :> QueryParam "eid" CID :> HTMLPageEndpoint "changelog"

changelogEndpoint :: Proxy ChangelogEndpoint
changelogEndpoint = Proxy

changelogHref :: Maybe CID -> Attribute
changelogHref cid =
    href_ $ uriToText $ safeLink jasenrekisteriAPI changelogEndpoint cid

type MemberlogEndpoint = JasenrekisteriAuth :> "member-log" :> Capture "id" MemberId :> HTMLPageEndpoint "memberlog"

memberlogEndpoint :: Proxy MemberlogEndpoint
memberlogEndpoint = Proxy

memberlogHref :: MemberId -> Attribute
memberlogHref memberId =
    href_ $ uriToText $ safeLink jasenrekisteriAPI memberlogEndpoint memberId

type SearchCsvEndpoint = JasenrekisteriAuth :> "search.csv" :> QueryParam "query" SearchQuery :> Get '[(CSV', SemiColonOpts)] [Contact]

searchCsvEndpoint :: Proxy SearchCsvEndpoint
searchCsvEndpoint = Proxy

searchCsvHref :: SearchQuery -> Attribute
searchCsvHref query =
    href_ $ uriToText $ safeLink jasenrekisteriAPI searchCsvEndpoint (Just query)

type SearchXlsxEndpoint = JasenrekisteriAuth :> "search.xlsx" :> QueryParam "query" SearchQuery :> Get '[XLSX] SearchResult

searchXlsxEndpoint :: Proxy SearchXlsxEndpoint
searchXlsxEndpoint = Proxy

searchXlsxHref :: SearchQuery -> Attribute
searchXlsxHref query =
    href_ $ uriToText $ safeLink jasenrekisteriAPI searchXlsxEndpoint (Just query)

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

uriToText :: URI -> Text
uriToText uri = view packed $ "/" <> uriPath uri <> uriQuery uri

-------------------------------------------------------------------------------
-- Cassave
-------------------------------------------------------------------------------

data SemiColonOpts
instance EncodeOpts SemiColonOpts where
    encodeOpts _ = defaultEncodeOptions
        { encDelimiter = fromIntegral $ fromEnum ';'
        }
