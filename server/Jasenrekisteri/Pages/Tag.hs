{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Jasenrekisteri.Pages.Tag (tagPage) where

import Control.Lens
import Control.Lens.Att
import Data.Set.Lens    (setOf)
import Futurice.IdMap   (key)
import Futurice.Prelude
import Prelude ()

import qualified Data.Set       as Set
import qualified Futurice.Graph as G

import Jasenrekisteri.HtmlUtils
import Jasenrekisteri.Person
import Jasenrekisteri.Tag
import Jasenrekisteri.World

tagPage :: World -> TagName -> HtmlPage "tag"
tagPage world tn =
    let tag = world ^. worldTags . att tn
    in tagPage' world tag

-- TODO: use closure fields
tagPage' :: World -> Tag -> HtmlPage "tag"
tagPage' world tag = template' ("Tagi: " <> tn ^. _TagName) $ do
    subheader_ "Alatagit"
    tagList_ (world ^.. subtagsFold . filtered (\subtag -> subtag ^. tagName /= tn))
    subheader_ "Jäsenet"
    memberList_ (world ^.. membersFold)
  where
    tn = tag ^. tagName

    subtagNames = setOf (subtagsFold . key) world

    subtagsFold :: Fold World Tag
    subtagsFold
        = worldTags
        . _TagHierarchy
        . to (flip G.closure [tn])
        . _Just . folded

    membersFold :: Fold World Person
    membersFold
        = worldMembers
        . folded
        . filtered (\member -> overlaps (member ^. personTags) subtagNames)

    overlaps :: TagNames -> Set TagName -> Bool
    overlaps tns tns' = not $ null $ Set.intersection
        (tns ^. _TagNames) tns'
