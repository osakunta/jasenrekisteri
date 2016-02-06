{-# LANGUAGE OverloadedStrings #-}
module Jasenrekisteri.Pages.Tags (tagsPage) where

import Prelude        ()
import Prelude.Compat

import Data.Foldable (traverse_)
import Data.List     (sort)
import Lucid

import qualified Data.HashMap.Strict as HM

import Jasenrekisteri.Context
import Jasenrekisteri.HtmlUtils
import Jasenrekisteri.Tag

tagsPage :: JasenContext -> Html ()
tagsPage ctx = template' "Tagit" $ do
    h2_ "Enemmän kuin yksi"
    h2_ "Yksikkötagit"
    h2_ "TODO"
    ul_ [class_ "tags"] $
        traverse_ (li_ . renderTag (ctxColours ctx)) (sort . HM.keys . getChildTags $ ctxTags ctx)
