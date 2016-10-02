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

type HTMLEndpoint = Get '[HTML] (Html ())

type JasenrekisteriAPI =
    HTMLEndpoint
    :<|> "members" :> HTMLEndpoint
    :<|> MemberEndpoint
    :<|> "tags" :> HTMLEndpoint
    -- TODO: tag
    :<|> "logout" :> HTMLEndpoint
    :<|> Raw

jasenrekisteriAPI :: Proxy JasenrekisteriAPI
jasenrekisteriAPI = Proxy

type MemberEndpoint = "member" :> Capture "id" PersonId :> HTMLEndpoint

memberEndpoint :: Proxy MemberEndpoint
memberEndpoint = Proxy

memberHref :: PersonId -> Attribute
memberHref personId =
    href_ $ uriToText $ safeLink jasenrekisteriAPI memberEndpoint personId

uriToText :: URI -> Text
uriToText uri = view packed $ "/" <> uriPath uri <> uriQuery uri
