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
import Futurice.IdMap    (key)
import SatO.AcademicYear
import SatO.Foundation

import qualified Data.Aeson         as A
import qualified Data.Text          as T
import qualified Data.Text.Encoding as TE

import SatO.Jasenrekisteri.API
import SatO.Jasenrekisteri.Member
import SatO.Jasenrekisteri.Session
import SatO.Jasenrekisteri.Tag
import SatO.Jasenrekisteri.World

template :: GoogleClientId -> Html () -> Html () -> Html () -> HtmlPage sym
template gcid title nav inner = page_ gcid title $ do
    body_ $ do
        header_ $ do
            nav
            row_ $ large_ 12 $ h1_ title
        section_ inner

template' :: GoogleClientId -> Day -> LoginUser -> Html () -> Html () -> HtmlPage sym
template' gcid today lu title = template gcid title $ navigation today lu

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
                a_ [tagHref Nothing ayearTag] ayearTag
                ul_ [ class_ "menu" ] $ do
                    li_ $ a_ [tagHref Nothing ayearTag'] ayearTag'
                    li_ $ a_ [tagHref Nothing ayearTag''] ayearTag''
                    li_ $ a_ [tagHref (Just ColumnRoom) "talo"] "Talo"
            li_ $ a_ [changelogHref Nothing ] "Muutosloki"
            li_ $ a_ [searchHref Nothing Nothing] "Haku"
        div_ [ class_ "top-bar-right" ] $ ul_ [ class_ "menu" ] $ do
            li_ $ input_ [ class_ "search", placeholder_ "hae käyttäjä tai tägi" ]
            li_ [ class_ "menu-text" ] $ toHtml $ "Terve " <> getLoginUser lu
            -- li_ $ div_ [ class_ "g-signin2", data_ "onsuccess" "onSignIn" ] $ pure ()
            li_ $ a_ [ href_ "#", id_ "logout-link" ] "Ulos"
  where
    ayear :: Integer
    ayear = academicYear today

    ayearTag :: IsString a => a
    ayearTag = fromString $ show ayear ++ "-" ++ show (succ ayear)

    ayearTag' :: IsString a => a
    ayearTag' = fromString $ show (pred ayear) ++ "-" ++ show ayear

    ayearTag'' :: IsString a => a
    ayearTag'' = fromString $ show (pred $ pred ayear) ++ "-" ++ show (pred ayear)

page404 :: GoogleClientId -> Day -> LoginUser -> HtmlPage sym
page404 gcid today lu = template' gcid today lu "ei löydy" $ pure ()

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
    a_ [tagHref Nothing name, class_ $ "jrek-tag label " <> lblColour ] $ toHtml t
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
    -> (Column -> Attribute)
    -> Column
    -> Bool        -- ^ whether to show address
    -> [Tag]       -- ^ tag to show in the list
    -> [Member]
    -> HtmlT m ()
memberList_ today columnHref column hasTalo ts ps = do
    row_ $ do
        largemed_ 6 $ toHtml $  "Yhteensä: " <> (show $ length ps')
        largemed_ 6 $ label_ $ do
            "Suodata: "
            input_ [ type_ "text", id_ "member-filter" ]
    row_ . large_ 12 $ table_ [ id_ "member-list", class_ "hover" ] $ do
        thead_ $ tr_ $ do
            th_ $ a_ [ columnHref ColumnName ] $ "Nimi"
            when (isn't _Empty ts) $
                th_ $ a_ [ columnHref columnTags ] $ "Tagit"
            th_ $ ayearTag
            when hasTalo $ th_ $ a_ [ columnHref ColumnRoom ] $ "Huone"
        tbody_ $ for_ ps' $ \member -> do
            let memberId = member ^. key
            let needle = T.toLower
                  $ member ^. memberFullName
            tr_ [ data_ "member-haystack" needle ] $ do
                td_ $ a_ [ memberHref memberId ] $ member ^. memberShortNameHtml
                when (isn't _Empty ts) $ td_ $
                    tagList_ (memberTags' member)
                td_ $ tagCheckbox member ayearTag
                when hasTalo $ td_ $ toHtml $ member ^. memberTaloAddress
  where
    columnTags = case column of
        ColumnTags -> ColumnTagsDesc
        _          -> ColumnTags


    sortOnColumn :: [Member] -> [Member]
    sortOnColumn = case column of
        ColumnName     -> sortOn (view memberSortKey)
        ColumnTags     -> sortOn memberTags'
        ColumnTagsDesc -> reverse . sortOn memberTags'
        ColumnRoom     -> sortOn (view memberAddress)

    memberTags' :: Member -> [Tag]
    memberTags' member =
        ts ^.. folded . filtered (\t -> member ^. memberTags . contains (t ^. key))

    ps' = sortOnColumn ps

    ayear :: Integer
    ayear = academicYear today

    ayearTag :: IsString a => a
    ayearTag = fromString $ show ayear ++ "-" ++ show (succ ayear)

tagCheckbox :: Monad m => Member -> TagName -> HtmlT m ()
tagCheckbox member tn = label_ $ do
   checkbox_
        (member ^. memberTags . contains tn)
        [ data_ "tag" d ]
   (toHtml $ tn ^. _TagName)
 where
   memberId = member ^. key
   json = A.object [ "tagName" A..= tn, "memberId" A..= memberId ]
   d = TE.decodeUtf8 $ A.encode json ^. strict
