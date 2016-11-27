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
    tagnameList_,
    -- * Members
    memberList_,
    tagCheckbox,
    -- * Re-export
    module SatO.Foundation,
    ) where

import Prelude ()
import Futurice.Prelude
import Control.Lens
import Control.Lens.Att
import Data.Ord          (comparing)
import Futurice.IdMap    (key)
import SatO.AcademicYear
import SatO.Foundation

import qualified Data.Aeson         as A
import qualified Data.Text          as T
import qualified Data.Text.Encoding as TE

import SatO.Jasenrekisteri.API
import SatO.Jasenrekisteri.Person
import SatO.Jasenrekisteri.Session
import SatO.Jasenrekisteri.Tag
import SatO.Jasenrekisteri.World

template :: Html () -> Html () -> Html () -> HtmlPage sym
template title nav inner = page_ title $ do
    body_ $ do
        header_ $ do
            nav
            row_ $ large_ 12 $ h1_ title
        section_ inner

template' :: Day -> LoginUser -> Html () -> Html () -> HtmlPage sym
template' today lu title = template title $ navigation today lu

-- http://foundation.zurb.com/sites/docs/top-bar.html
navigation :: Monad m => Day -> LoginUser -> HtmlT m ()
navigation today lu = do
    div_ [ class_ "top-bar" ] $ do
        div_ [ class_ "top-bar-left" ] $ ul_ [ class_ "dropdown menu", data_ "dropdown-menu" "" ] $ do
            li_ [ class_ "menu-text"] $ do
                "Jäsenrekisteri"
                sup_ "2"
            li_ $ a_ [href_ "/"] "Jäsenet"
            li_ $ a_ [newMemberHref] "Uusi"
            li_ $ a_ [href_ "/tags"] "Tägit"
            li_ [ class_ "is-dropdown-submenu-parent"] $ do
                a_ [tagHref ayearTag] ayearTag
                ul_ [ class_ "menu" ] $ do
                    li_ $ a_ [tagHref ayearTag'] ayearTag'
                    li_ $ a_ [tagHref ayearTag''] ayearTag''
                    li_ $ a_ [tagHref "talo"] "Talo"
            li_ $ a_ [changelogHref Nothing ] "Muutosloki"
            li_ $ a_ [href_ "/search" ] "Haku"
        div_ [ class_ "top-bar-right" ] $ ul_ [ class_ "menu" ] $ do
            li_ $ input_ [ class_ "search", placeholder_ "hae käyttäjä tai tägi" ]
            li_ [ class_ "menu-text" ] $ toHtml $ "Terve " <> getLoginUser lu
  where
    ayear :: Integer
    ayear = academicYear today 

    ayearTag :: IsString a => a
    ayearTag = fromString $ show ayear ++ "-" ++ show (succ ayear)

    ayearTag' :: IsString a => a
    ayearTag' = fromString $ show (pred ayear) ++ "-" ++ show ayear

    ayearTag'' :: IsString a => a
    ayearTag'' = fromString $ show (pred $ pred ayear) ++ "-" ++ show (pred ayear)

page404 :: Day -> LoginUser -> HtmlPage sym
page404 today lu = template' today lu "ei löydy" $ pure ()

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
    colour = (tag ^. tagColour) `mod` 12
    lblColour = "lbl" <> (show colour ^. packed)

tagLink_ :: Monad m => Tag -> HtmlT m ()
tagLink_ tag = tagLink' tag (tag ^. key . _TagName)

tagList_ :: Monad m => [Tag] -> HtmlT m ()
tagList_ = row_ . large_ 12 . traverse_ tagLink_

tagnameList_ :: Monad m => World -> [TagName] -> HtmlT m ()
tagnameList_ world ts = traverse_ (tagNameLink_ world) ts
-------------------------------------------------------------------------------
-- Members
-------------------------------------------------------------------------------

memberList_
    :: Monad m
    => Day
    -> [Tag]       -- ^ tag to show in the list
    -> [Person]
    -> HtmlT m ()
memberList_ today ts ps = do
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
            th_ $ ayearTag
        tbody_ $ for_ ps' $ \person -> do
            let memberId = person ^. key
            let needle = T.toLower
                  $ person ^. personFullName
            tr_ [ data_ "member-haystack" needle ] $ do
                td_ $ a_ [ memberHref memberId ] $ person ^. personFullNameHtml
                when (isn't _Empty ts) $ td_ $
                    tagList_ (ts ^.. folded . filtered (\t -> person ^. personTags . contains (t ^. key)))
                td_ $ tagCheckbox person ayearTag
  where
    ps' = sortBy (comparing _personFirstNames <> comparing _personLastName) ps

    ayear :: Integer
    ayear = academicYear today 

    ayearTag :: IsString a => a
    ayearTag = fromString $ show ayear ++ "-" ++ show (succ ayear)

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
