{-# LANGUAGE OverloadedStrings #-}
module Jasenrekisteri.Pages.Tags (tagsPage) where

import Control.Lens
import Futurice.Prelude
import Prelude ()

import Data.Monoid    (Sum (..))
import Futurice.IdMap (key)

import Jasenrekisteri.HtmlUtils
import Jasenrekisteri.Tag
import Jasenrekisteri.World

tagsPage :: World -> Html ()
tagsPage world = template' "Tagit" $ do
    let tagsWithCounts = world ^.. worldTags . ifoldedTagHierarchy . to withCount
    subheader_ "Enemmän kuin yksi"
    row_ $ large_ 12 $ for_ tagsWithCounts $ \(tag, Sum count) ->
        when (count > 1) $ do
            let countText = show count ^. packed
            let name = tag ^. key . _TagName
            tagLink' tag (name <> " (" <> countText <> ")")
    subheader_ "Yksikkötagit"
    row_ $ large_ 12 $ for_ tagsWithCounts $ \(tag, Sum count) ->
        when (count == 1) $ tagLink_ tag
    subheader_ "Tyhjät"
    row_ $ large_ 12 $ for_ tagsWithCounts $ \(tag, Sum count) ->
        when (count == 0) $ tagLink_ tag
  where
    withCount :: Tag -> (Tag, Sum Int)
    withCount tag = (tag, world ^. worldTagPersonCount . ix (tag ^. key))
