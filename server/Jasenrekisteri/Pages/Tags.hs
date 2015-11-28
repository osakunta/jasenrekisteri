{-# LANGUAGE OverloadedStrings #-}
module Jasenrekisteri.Pages.Tags (tagsPage) where

import Prelude        ()
import Prelude.Compat

import Data.Foldable (traverse_)
import Lucid

import Jasenrekisteri.HtmlUtils
import Jasenrekisteri.Tag

tagsPage :: Tags -> Html ()
tagsPage (Tags tags) = template' "Tagit" $ do
	h2_ "Enemmän kuin yksi"
	h2_ "Yksikkötagit"
	h2_ "TODO"
	ul_ [class_ "tags"] $
		traverse_ (li_ . renderTag) tags
