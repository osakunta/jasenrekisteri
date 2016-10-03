{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Jasenrekisteri.Pages.Tag (tagPage) where

import Control.Lens
import Control.Lens.Att
import Data.Set.Lens    (setOf)
import Futurice.Prelude
import Prelude ()

import qualified Futurice.Graph as G
import qualified Data.Set as Set

import Futurice.IdMap (key)
import Lucid hiding (for_)

import Jasenrekisteri.API
import Jasenrekisteri.HtmlUtils
import Jasenrekisteri.Person
import Jasenrekisteri.Tag
import Jasenrekisteri.World

tagPage :: World -> TagName -> Html ()
tagPage world tn =
    let tag = world ^. worldTags . att tn
    in tagPage' world tag

tagPage' :: World -> Tag -> Html ()
tagPage' world tag = template' ("Tagi: " <> tn ^. _TagName) $ do
    h2_ "Alatagit"
    ul_ $ forOf_ (subtagsFold . filtered (\subtag -> subtag ^. tagName /= tn)) world $ \subtag ->
        li_ $ tagLink_ subtag
    h2_ "Jäsenet"
    ul_ $ forOf_ membersFold world $ \person ->
        memberHtml (person ^. key) person
  where
    tn = tag ^. tagName

    subtagNames = setOf (subtagsFold . key) world

    subtagsFold :: Fold World Tag
    subtagsFold
        = worldTags
        . _TagHierarchy
        . to (flip G.closure [tn])
        . _Just . folded
--        . filtered (\subtag -> subtag ^. tagName /= tn)

    membersFold :: Fold World Person
    membersFold
        = worldMembers
        . folded
        . filtered (\member -> overlaps (member ^. personTags) subtagNames)

    overlaps :: TagNames -> Set TagName -> Bool
    overlaps tns tns' = not $ null $ Set.intersection
        (tns ^. _TagNames) tns'

-- TODO: move to html utils
memberHtml :: PersonId -> Person -> Html ()
memberHtml i Person{..} =
    li_ $ a_ [memberHref i] $ do
        span_ [class_ "etu"] $ toHtml _personFirstNames
        " "
        span_ [class_ "suku"] $ toHtml _personLastName
