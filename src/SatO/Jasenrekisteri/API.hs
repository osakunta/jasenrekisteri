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

type HTMLPageEndpoint sym = Get '[HTML] (HtmlPage sym)

-------------------------------------------------------------------------------
-- API
-------------------------------------------------------------------------------

type JasenrekisteriAPI =
    Session () :> HTMLPageEndpoint "members"
    :<|> MemberEndpoint
    :<|> Session () :> "tags" :> HTMLPageEndpoint "tags"
    :<|> TagEndpoint
    :<|> Session () :> "search" :> QueryParam "query" SearchQuery :> HTMLPageEndpoint "search"
    :<|> "login" :> ReqBody '[JSON] LoginData :> Post '[JSON] (Maybe UUID)
    :<|> Session () :> "logout" :> HTMLPageEndpoint "logout"
    :<|> "command" :> ReqBody '[JSON] Command :> Post '[JSON] Text
    :<|> Raw

jasenrekisteriAPI :: Proxy JasenrekisteriAPI
jasenrekisteriAPI = Proxy

-------------------------------------------------------------------------------
-- Endpoints
-------------------------------------------------------------------------------

type MemberEndpoint = Session () :> "member" :> Capture "id" PersonId :> HTMLPageEndpoint "member"

memberEndpoint :: Proxy MemberEndpoint
memberEndpoint = Proxy

memberHref :: PersonId -> Attribute
memberHref personId =
    href_ $ uriToText $ safeLink jasenrekisteriAPI memberEndpoint personId

type TagEndpoint = Session () :> "tag" :> Capture "tag" TagName :> HTMLPageEndpoint "tag"

tagEndpoint :: Proxy TagEndpoint
tagEndpoint = Proxy

tagHref :: TagName -> Attribute
tagHref tn =
    href_ $ uriToText $ safeLink jasenrekisteriAPI tagEndpoint tn

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

uriToText :: URI -> Text
uriToText uri = view packed $ "/" <> uriPath uri <> uriQuery uri
