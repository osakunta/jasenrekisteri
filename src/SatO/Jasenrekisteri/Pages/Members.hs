{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module SatO.Jasenrekisteri.Pages.Members (membersPage) where

import Prelude ()
import Futurice.Prelude
import Control.Lens

import Control.Monad.Reader (ask)

import SatO.Jasenrekisteri.API
import SatO.Jasenrekisteri.Endpoints
import SatO.Jasenrekisteri.Markup
import SatO.Jasenrekisteri.Session
import SatO.Jasenrekisteri.World

membersPage :: LoginUser -> QueryM (HtmlPage "members")
membersPage lu = ask <&> \(world, today) -> template' today lu "Jäsenet" $ do
    memberList_ today (\_ -> membersHref) ColumnName False [] (world ^.. worldMembers . folded)
-- TODO: use top tags?
