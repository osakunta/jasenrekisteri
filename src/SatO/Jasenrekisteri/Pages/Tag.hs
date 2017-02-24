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

import SatO.Jasenrekisteri.API
import SatO.Jasenrekisteri.Endpoints
import SatO.Jasenrekisteri.Markup
import SatO.Jasenrekisteri.Member
import SatO.Jasenrekisteri.Session
import SatO.Jasenrekisteri.Tag
import SatO.Jasenrekisteri.World

tagPage :: LoginUser -> Maybe Column -> TagName -> QueryM (HtmlPage "tag")
tagPage lu mcolumn tn = do
    (world, today, gcid) <- ask
    let tag = world ^. worldTags . att tn
    pure $ tagPage' gcid today lu world mcolumn tag

-- TODO: use closure fields
tagPage' :: GoogleClientId -> Day -> LoginUser -> World -> Maybe Column -> Tag -> HtmlPage "tag"
tagPage' gcid today lu world mcolumn tag = template' gcid today lu ("Tagi: " <> toHtml (tn ^. _TagName)) $ do
    when (not $ null tags) $ do
        subheader_ "Alatägit"
        tagList_ tags
    when (not $ null parentTags) $ do
        subheader_ "Ylätägit"
        tagList_ parentTags
    subheader_ "Jäsenet"
    memberList_ today (\c -> tagHref (Just c) tn) column hasTalo tags (world ^.. membersFold)
  where
    tn = tag ^. tagName

    column = case mcolumn of
        Just ColumnRoom | hasTalo               -> ColumnRoom
        Just ColumnTags | isn't _Empty tags     -> ColumnTags
        Just ColumnTagsDesc | isn't _Empty tags -> ColumnTagsDesc
        _                                       -> ColumnName

    tags :: [Tag]
    tags = world ^.. subtagsFold . filtered (\subtag -> subtag ^. tagName /= tn)

    parentTags :: [Tag]
    parentTags = world ^.. parenttagsFold . filtered (\parentTag -> parentTag ^. tagName /= tn)

    subtagNames = setOf (subtagsFold . key) world

    hasTalo = p tn || any (p . view tagName) tags
      where
        p t = t == "talo" || t == "osakehuoneisto"

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

    membersFold :: Fold World Member
    membersFold
        = worldMembers
        . folded
        . filtered (\member -> overlaps (member ^. memberTags) subtagNames)

    overlaps :: TagNames -> Set TagName -> Bool
    overlaps tns tns' = not $ null $ Set.intersection
        (tns ^. _TagNames) tns'
