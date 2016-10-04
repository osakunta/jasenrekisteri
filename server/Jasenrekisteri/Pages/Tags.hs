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
    subheader_ "Enemmän kuin yksi"
    subheader_ "Yksikkötagit"
    subheader_ "TODO"
    tagList_ (world ^.. worldTags . ifoldedTagHierarchy)
