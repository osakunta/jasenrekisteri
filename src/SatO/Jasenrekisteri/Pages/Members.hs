{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module SatO.Jasenrekisteri.Pages.Members (membersPage) where

import Control.Lens
import Futurice.Prelude
import Prelude ()

import Control.Monad.Reader (ask)

import SatO.Jasenrekisteri.Endpoints
import SatO.Jasenrekisteri.Markup
import SatO.Jasenrekisteri.Session
import SatO.Jasenrekisteri.World

membersPage :: LoginUser -> QueryM (HtmlPage "members")
membersPage lu = ask <&> \(world, today) -> template' today lu "Jäsenet" $ do
    memberList_ today [] (world ^.. worldMembers . folded)
-- TODO: use top tags?
