{-# LANGUAGE OverloadedStrings #-}
module Jasenrekisteri.HtmlUtils (
    template',
    -- * Headers
    subheader_,
    -- * Tags
    tagLink_,
    tagLink',
    tagNameLink_,
    tagList_,
    tagnameList_,
    -- * Members
    memberList_,
    -- * Re-export
    module SatO.Foundation,
    ) where

import Control.Lens
import Control.Lens.Att
import Data.Ord         (comparing)
import Futurice.IdMap   (key)
import Futurice.Prelude
import Prelude ()
import SatO.Foundation

{-
import qualified Data.Aeson as A
-}
import qualified Data.Text as T

import Jasenrekisteri.API
import Jasenrekisteri.Person
import Jasenrekisteri.Tag
import Jasenrekisteri.World

template :: Text -> Html () -> Html () -> Html ()
template title nav inner = toHtml $ page_ title $ do
    body_ $ do
        header_ $ do
            nav
            row_ $ large_ 12 $ h1_ $ toHtml title
        section_ inner

template' :: Text -> Html () -> Html ()
template' title = template title navigation

-- http://foundation.zurb.com/sites/docs/top-bar.html
navigation :: Monad m => HtmlT m ()
navigation = do
    div_ [ class_ "top-bar" ] $ do
        div_ [ class_ "top-bar-left" ] $ ul_ [ class_ "dropdown menu" ] $ do
            li_ [ class_ "menu-text"] $ do
                "Jäsenrekisteri"
                sup_ "2"
            li_ $ a_ [href_ "/members"] "Jäsenet"
            li_ $ a_ [href_ "/tags" ] "Tagit"
            li_ $ a_ [tagHref "2014-2015"] "2014-2014"
            li_ $ a_ [tagHref "2015-2016"] "2015-2016"
            li_ $ a_ [tagHref "2016-2017"] "2016-2017"
            li_ $ a_ [tagHref "talo"] "Talo"
            li_ $ a_ [href_ "/search" ] "Haku"
        div_ [ class_ "top-bar-right" ] $ ul_ [ class_ "dropdown menu" ] $ do
            li_ [ class_ "menu-text" ] $ "Hello Urho!"
            li_ $ a_ [href_ "/logout" ] "Kirjaudu ulos"

-------------------------------------------------------------------------------
-- Subheader
-------------------------------------------------------------------------------

subheader_ :: Monad m => Text -> HtmlT m ()
subheader_ = row_ . large_ 12 . h2_ . toHtml

-------------------------------------------------------------------------------
-- Tags
-------------------------------------------------------------------------------

tagNameLink_ :: Monad m => World -> TagName -> HtmlT m ()
tagNameLink_ world tagname = tagLink_ (world ^. worldTags . att tagname)

tagLink' :: Monad m => Tag -> Text -> HtmlT m ()
tagLink' tag t = do
    -- TODO: do colours
    a_ [tagHref name] $ span_ [ class_ "label" ] $ toHtml t
    " "
  where
    name = tag ^. key
    {-
    colour = tag ^. tagColour
    colourAttr | colour == 0 = ""
               | otherwise   = " tag" <> T.pack (show colour)
    -}

tagLink_ :: Monad m => Tag -> HtmlT m ()
tagLink_ tag = tagLink' tag (tag ^. key . _TagName)

tagList_ :: Monad m => [Tag] -> HtmlT m ()
tagList_ = row_ . large_ 12 . traverse_ tagLink_

tagnameList_ :: Monad m => World -> [TagName] -> HtmlT m ()
tagnameList_ world = row_ . large_ 12 . traverse_ (tagNameLink_ world)

-------------------------------------------------------------------------------
-- Members
-------------------------------------------------------------------------------

memberList_ :: Monad m => [Person] -> HtmlT m ()
memberList_ ps = do
    row_ $ do
        largemed_ 6 $ toHtml $  "Yhteensä: " <> (show $ length ps')
        largemed_ 6 $ label_ $ do
            "Haku: "
            input_ [ type_ "text", id_ "member-filter" ]
    row_ . large_ 12 $ table_ [ id_ "member-list", class_ "hover" ] $ do
        thead_ $ tr_ $ do
            th_ $ "Nimi"
            th_ $ "2016-2017"
        tbody_ $ for_ ps' $ \person -> do
            let needle = T.toLower
                  $ person ^. personFirstNames
                  <> " " <> person ^. personLastName
            tr_ [ data_ "member-haystack" needle ] $ do
                td_ $ a_ [ memberHref $ person ^. key ] $ do
                    span_ [class_ "etu"] $ toHtml $ person ^. personFirstNames
                    " "
                    span_ [class_ "suku"] $ toHtml $ person ^. personLastName
                td_ $ do
                    label_ $ do
                        checkbox_ (person ^. personTags . contains "2016-2017") []
                        "2016-2017"
  where
    ps' = sortBy (comparing _personFirstNames <> comparing _personLastName) ps
