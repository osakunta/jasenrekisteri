{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Jasenrekisteri.API where

import Lucid
import Servant
import Servant.HTML.Lucid

import Jasenrekisteri.Types

type HTMLEndpoint = Get '[HTML] (Html ())

type JasenrekisteriAPI =
    HTMLEndpoint
    :<|> "members" :> HTMLEndpoint
    :<|> "member" :> Capture "id" UserId :> HTMLEndpoint
    :<|> "tags" :> HTMLEndpoint
    -- tag
    :<|> "logout" :> HTMLEndpoint
    :<|> Raw

jasenrekisteriAPI :: Proxy JasenrekisteriAPI
jasenrekisteriAPI = Proxy
