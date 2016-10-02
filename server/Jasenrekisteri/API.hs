{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Jasenrekisteri.API where

import Futurice.Prelude
import Prelude ()

import Lucid
import Servant
import Servant.HTML.Lucid

import Jasenrekisteri.Person
import Jasenrekisteri.Tag

-------------------------------------------------------------------------------
-- Temporary, use HtmlPage
-------------------------------------------------------------------------------

type HTMLEndpoint = Get '[HTML] (Html ())

-------------------------------------------------------------------------------
-- API
-------------------------------------------------------------------------------

type JasenrekisteriAPI =
    HTMLEndpoint
    :<|> "members" :> HTMLEndpoint
    :<|> MemberEndpoint
    :<|> "tags" :> HTMLEndpoint
    :<|> TagEndpoint 
    :<|> "logout" :> HTMLEndpoint
    :<|> Raw

jasenrekisteriAPI :: Proxy JasenrekisteriAPI
jasenrekisteriAPI = Proxy

-------------------------------------------------------------------------------
-- Endpoints
-------------------------------------------------------------------------------

type MemberEndpoint = "member" :> Capture "id" PersonId :> HTMLEndpoint

memberEndpoint :: Proxy MemberEndpoint
memberEndpoint = Proxy

memberHref :: PersonId -> Attribute
memberHref personId =
    href_ $ uriToText $ safeLink jasenrekisteriAPI memberEndpoint personId

type TagEndpoint = "tag" :> Capture "tag" TagName :> HTMLEndpoint

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
