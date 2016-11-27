{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module SatO.Jasenrekisteri.Pages.Tags (tagsPage) where

import Control.Lens
import Futurice.Prelude
import Prelude ()

import Control.Monad.Reader (ask)
import Data.Monoid    (Sum (..))
import Futurice.IdMap (key)

import SatO.Jasenrekisteri.Endpoints
import SatO.Jasenrekisteri.Markup
import SatO.Jasenrekisteri.Session
import SatO.Jasenrekisteri.Tag
import SatO.Jasenrekisteri.World

tagsPage :: LoginUser -> QueryM (HtmlPage "tags")
tagsPage lu = ask <&> \(world, today) -> template' today lu "Tagit" $ do
    let withCount :: Tag -> (Tag, Sum Int)
        withCount tag = (tag, world ^. worldTagPersonCount . ix (tag ^. key))

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
