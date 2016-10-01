{-# LANGUAGE OverloadedStrings #-}
module Jasenrekisteri.Pages.Tags (tagsPage) where

import Futurice.Prelude
import Prelude ()

import Lucid         hiding (for_)

import Jasenrekisteri.Context
import Jasenrekisteri.HtmlUtils
-- import Jasenrekisteri.Tag

tagsPage :: JasenContext -> Html ()
tagsPage _ctx = template' "Tagit" $ do
    h2_ "Enemmän kuin yksi"
    h2_ "Yksikkötagit"
    h2_ "TODO"
    ul_ [class_ "tags"] $
        -- TODO: render tag
        for_ [()] $ (li_ . toHtml . show)
-- (sort . HM.keys . getChildTags $ ctxTags ctx)
