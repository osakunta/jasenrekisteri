{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module SatO.Jasenrekisteri.Pages.Tag (tagPage) where

import Prelude ()
import Futurice.Prelude
import Control.Lens
import Control.Lens.Att
import Control.Monad.Reader (ask)
import Data.Set.Lens        (setOf)
import Futurice.IdMap       (key)

import qualified Data.Set       as Set
import qualified Futurice.Graph as G

import SatO.Jasenrekisteri.Endpoints
import SatO.Jasenrekisteri.Markup
import SatO.Jasenrekisteri.Person
import SatO.Jasenrekisteri.Session
import SatO.Jasenrekisteri.Tag
import SatO.Jasenrekisteri.World

tagPage :: LoginUser -> TagName -> QueryM (HtmlPage "tag")
tagPage lu tn = do
    (world, today) <- ask
    let tag = world ^. worldTags . att tn
    pure $ tagPage' today lu world tag

-- TODO: use closure fields
tagPage' :: Day -> LoginUser -> World -> Tag -> HtmlPage "tag"
tagPage' today lu world tag = template' today lu ("Tagi: " <> toHtml (tn ^. _TagName)) $ do
    when (not $ null tags) $ do
        subheader_ "Alatägit"
        tagList_ tags
    when (not $ null parentTags) $ do
        subheader_ "Ylätägit"
        tagList_ parentTags
    subheader_ "Jäsenet"
    memberList_ today tags (world ^.. membersFold)
  where
    tn = tag ^. tagName

    tags :: [Tag]
    tags = world ^.. subtagsFold . filtered (\subtag -> subtag ^. tagName /= tn)

    parentTags :: [Tag]
    parentTags = world ^.. parenttagsFold . filtered (\parentTag -> parentTag ^. tagName /= tn)

    subtagNames = setOf (subtagsFold . key) world

    subtagsFold :: Fold World Tag
    subtagsFold
        = worldTags
        . _TagHierarchy
        . to (flip G.closure [tn])
        . _Just . folded

    parenttagsFold :: Fold World Tag
    parenttagsFold
        = worldTags
        . _TagHierarchy
        . to (flip G.revClosure [tn])
        . _Just . folded

    membersFold :: Fold World Person
    membersFold
        = worldMembers
        . folded
        . filtered (\member -> overlaps (member ^. personTags) subtagNames)

    overlaps :: TagNames -> Set TagName -> Bool
    overlaps tns tns' = not $ null $ Set.intersection
        (tns ^. _TagNames) tns'
