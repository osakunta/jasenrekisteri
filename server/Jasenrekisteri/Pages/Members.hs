{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Jasenrekisteri.Pages.Members (membersPage) where

import Futurice.Prelude
import Prelude ()

import Jasenrekisteri.HtmlUtils
import Jasenrekisteri.World

membersPage :: World -> Html ()
membersPage world = template' "Jäsenet" $ do
    subheader_ "Jäsenet"
    memberList_ (world ^.. worldMembers . folded)
