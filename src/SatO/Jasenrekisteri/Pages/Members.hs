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
import SatO.Jasenrekisteri.World

membersPage :: QueryM (HtmlPage "members")
membersPage = ask <&> \world -> template' "Jäsenet" $ do
    memberList_ [] (world ^.. worldMembers . folded)
-- TODO: use top tags?
