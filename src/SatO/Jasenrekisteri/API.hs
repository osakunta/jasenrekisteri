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
import Lucid
import SatO.Foundation    (HtmlPage)
import Servant
import Servant.HTML.Lucid

import SatO.Jasenrekisteri.Command
import SatO.Jasenrekisteri.Person
import SatO.Jasenrekisteri.SearchQuery
import SatO.Jasenrekisteri.Session
import SatO.Jasenrekisteri.Tag
import SatO.Jasenrekisteri.SearchData

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
    :<|> JasenrekisteriAuth :> "command" :> ReqBody '[JSON] (Command Proxy) :> Post '[JSON] Text
    :<|> MemberlogEndpoint
    :<|> JasenrekisteriAuth :> "search-data" :> Get '[JSON] [SearchItem]
    :<|> Raw

jasenrekisteriAPI :: Proxy JasenrekisteriAPI
jasenrekisteriAPI = Proxy

-------------------------------------------------------------------------------
-- Endpoints
-------------------------------------------------------------------------------

type MemberEndpoint = JasenrekisteriAuth :> "member" :> Capture "id" PersonId :> HTMLPageEndpoint "member"

memberEndpoint :: Proxy MemberEndpoint
memberEndpoint = Proxy

memberHrefText :: PersonId -> Text
memberHrefText personId =
    uriToText $ safeLink jasenrekisteriAPI memberEndpoint personId

memberHref :: PersonId -> Attribute
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

type MemberlogEndpoint = JasenrekisteriAuth :> "member-log" :> Capture "id" PersonId :> HTMLPageEndpoint "memberlog"

memberlogEndpoint :: Proxy MemberlogEndpoint
memberlogEndpoint = Proxy

memberlogHref :: PersonId -> Attribute
memberlogHref memberId =
    href_ $ uriToText $ safeLink jasenrekisteriAPI memberlogEndpoint memberId

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

uriToText :: URI -> Text
uriToText uri = view packed $ "/" <> uriPath uri <> uriQuery uri
