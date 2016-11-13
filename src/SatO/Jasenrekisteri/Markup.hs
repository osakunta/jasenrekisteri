{-# LANGUAGE OverloadedStrings #-}
module SatO.Jasenrekisteri.Markup (
    template,
    template',
    page404,
    -- * Headers
    subheader_,
    -- * Tags
    tagLink_,
    tagLink',
    tagNameLink_,
    tagList_,
    -- * Members
    memberList_,
    -- * Re-export
    module SatO.Foundation,
    ) where

import Prelude ()
import Futurice.Prelude
import Control.Lens
import Control.Lens.Att
import Data.Ord         (comparing)
import Futurice.IdMap   (key)
import SatO.Foundation

import qualified Data.Aeson         as A
import qualified Data.Text          as T
import qualified Data.Text.Encoding as TE

import SatO.Jasenrekisteri.API
import SatO.Jasenrekisteri.Person
import SatO.Jasenrekisteri.Session
import SatO.Jasenrekisteri.Tag
import SatO.Jasenrekisteri.World

template :: Text -> Html () -> Html () -> HtmlPage sym
template title nav inner = page_ title $ do
    body_ $ do
        header_ $ do
            nav
            row_ $ large_ 12 $ h1_ $ toHtml title
            -- TODO: remove me
            row_ $ large_ 12 $ div_ [ class_ "callout alert"] $ do
                b_ "HUOM!"
                " Muutokset eivät tallennu pysyvästi"
        section_ inner

template' :: LoginUser -> Text -> Html () -> HtmlPage sym
template' lu title = template title $ navigation lu

-- http://foundation.zurb.com/sites/docs/top-bar.html
navigation :: Monad m => LoginUser -> HtmlT m ()
navigation lu = do
    div_ [ class_ "top-bar" ] $ do
        div_ [ class_ "top-bar-left" ] $ ul_ [ class_ "dropdown menu" ] $ do
            li_ [ class_ "menu-text"] $ do
                "Jäsenrekisteri"
                sup_ "2"
            li_ $ a_ [href_ "/"] "Jäsenet"
            li_ $ a_ [href_ "/tags" ] "Tagit"
            li_ $ a_ [tagHref "2014-2015"] "2014-2014"
            li_ $ a_ [tagHref "2015-2016"] "2015-2016"
            li_ $ a_ [tagHref "2016-2017"] "2016-2017"
            li_ $ a_ [tagHref "talo"] "Talo"
            li_ $ a_ [href_ "/search" ] "Haku"
        div_ [ class_ "top-bar-right" ] $ ul_ [ class_ "dropdown menu" ] $ do
            li_ [ class_ "menu-text" ] $ toHtml $ "Terve " <> getLoginUser lu

page404 :: LoginUser -> HtmlPage sym
page404 lu = template' lu "ei löydy" $ pure ()

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
    a_ [tagHref name, class_ $ "jrek-tag label " <> lblColour ] $ toHtml t
    " "
  where
    name = tag ^. key
    colour = (tag ^. tagColour) `mod` 10
    lblColour = "lbl" <> (show colour ^. packed)

tagLink_ :: Monad m => Tag -> HtmlT m ()
tagLink_ tag = tagLink' tag (tag ^. key . _TagName)

tagList_ :: Monad m => [Tag] -> HtmlT m ()
tagList_ = row_ . large_ 12 . traverse_ tagLink_

-------------------------------------------------------------------------------
-- Members
-------------------------------------------------------------------------------

memberList_
    :: Monad m
    => [Tag]       -- ^ tag to show in the list
    -> [Person]
    -> HtmlT m ()
memberList_ ts ps = do
    row_ $ do
        largemed_ 6 $ toHtml $  "Yhteensä: " <> (show $ length ps')
        largemed_ 6 $ label_ $ do
            "Suodata: "
            input_ [ type_ "text", id_ "member-filter" ]
    row_ . large_ 12 $ table_ [ id_ "member-list", class_ "hover" ] $ do
        thead_ $ tr_ $ do
            th_ $ "Nimi"
            when (isn't _Empty ts) $
                th_ "Tagit"
            th_ $ "2016-2017"
        tbody_ $ for_ ps' $ \person -> do
            let memberId = person ^. key
            let needle = T.toLower
                  $ person ^. personFirstNames
                  <> " " <> person ^. personLastName
            tr_ [ data_ "member-haystack" needle ] $ do
                td_ $ a_ [ memberHref memberId ] $ do
                    span_ [class_ "etu"] $ toHtml $ person ^. personFirstNames
                    " "
                    span_ [class_ "suku"] $ toHtml $ person ^. personLastName
                when (isn't _Empty ts) $ td_ $
                    tagList_ (ts ^.. folded . filtered (\t -> person ^. personTags . contains (t ^. key)))
                td_ $ tagCheckbox person "2016-2017"
  where
    ps' = sortBy (comparing _personFirstNames <> comparing _personLastName) ps

tagCheckbox :: Monad m => Person -> TagName -> HtmlT m ()
tagCheckbox person tn = label_ $ do
   checkbox_
        (person ^. personTags . contains tn)
        [ data_ "tag" d ]
   (toHtml $ tn ^. _TagName)
 where
   memberId = person ^. key
   json = A.object [ "tagName" A..= tn, "memberId" A..= memberId ]
   d = TE.decodeUtf8 $ A.encode json ^. strict
