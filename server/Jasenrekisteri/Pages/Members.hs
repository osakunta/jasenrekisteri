{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Jasenrekisteri.Pages.Members (membersPage) where

import Futurice.Prelude
import Prelude ()

import Jasenrekisteri.HtmlUtils
import Jasenrekisteri.World

membersPage :: World -> HtmlPage "members"
membersPage world = template' "Jäsenet" $ do
    memberList_ (world ^.. worldMembers . folded)
