{-# LANGUAGE OverloadedStrings #-}
module Jasenrekisteri.Pages.Tags (tagsPage) where

import Control.Lens
import Futurice.Prelude
import Prelude ()

import Lucid hiding (for_)

import Jasenrekisteri.HtmlUtils
import Jasenrekisteri.World
import Jasenrekisteri.Tag

tagsPage :: World -> Html ()
tagsPage world = template' "Tagit" $ do
    h2_ "Enemmän kuin yksi"
    h2_ "Yksikkötagit"
    h2_ "TODO"
    ul_ [class_ "tags"] $
        iforOf_ (worldTags . ifoldedTagHierarchy) world $ \_tagname tag ->
            li_ $ tagLink_ tag
