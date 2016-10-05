{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module SatO.Jasenrekisteri.Pages.Members (membersPage) where

import Futurice.Prelude
import Prelude ()

import SatO.Jasenrekisteri.Markup
import SatO.Jasenrekisteri.World

membersPage :: World -> HtmlPage "members"
membersPage world = template' "Jäsenet" $ do
    memberList_ [] (world ^.. worldMembers . folded)
-- TODO: use top tags?
