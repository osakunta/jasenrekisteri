{-# LANGUAGE OverloadedStrings #-}
module Jasenrekisteri.HtmlUtils (
    template,
    template',
    tagNameLink_,
    tagLink_,
    ) where

import Control.Lens.Att
import Futurice.IdMap   (key)
import Futurice.Prelude
import Prelude ()

import Lucid

import qualified Data.Text as T

import Jasenrekisteri.API
import Jasenrekisteri.Tag
import Jasenrekisteri.World

template :: Text -> Html () -> Html () -> Html ()
template title nav inner = doctypehtml_ $ do
    head_ $ do
        meta_ [charset_ "utf-8"]
        title_ $ toHtml title
        link_ [rel_ "stylesheet", href_ "/style.css", type_ "text/css" ]
    body_ $ do
        header_ $ do
            h1_ $ toHtml title
            nav
        section_ inner

template' :: Text -> Html () -> Html ()
template' title = template title navigation

navigation :: Html ()
navigation = nav_ $ ul_ $ do
    li_ $ a_ [href_ "/members"] "Jäsenet"
    li_ $ a_ [href_ "/tag/2013-2014"] "2013-2014"
    li_ $ a_ [href_ "/tag/2014-2015"] "2014-2015"
    li_ $ a_ [href_ "/tag/2015-2016"] "2015-2016"
    li_ $ a_ [href_ "/tags"] "Tagit"
    li_ $ a_ [href_ "/algebra"] "Algebra"
    li_ [class_ "logout"] $ a_ [href_ "/logout"] "Kirjaudu ulos"

tagNameLink_ :: World -> TagName -> Html ()
tagNameLink_ world tagname = tagLink_ (world ^. worldTags . att tagname)

tagLink_ :: Tag -> Html ()
tagLink_ tag =
    a_ [class_ $ "tag" <> colourAttr, tagHref name] $ toHtml name
  where
    name = tag ^. key
    colour = tag ^. tagColour
    colourAttr | colour == 0 = ""
               | otherwise   = " tag" <> T.pack (show colour)
