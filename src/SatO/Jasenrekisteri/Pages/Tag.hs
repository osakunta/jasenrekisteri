{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module SatO.Jasenrekisteri.Pages.Tag (tagPage) where

import Control.Lens
import Control.Lens.Att
import Data.Set.Lens    (setOf)
import Futurice.IdMap   (key)
import Futurice.Prelude
import Prelude ()
import Control.Monad.Reader (ask)

import qualified Data.Set       as Set
import qualified Futurice.Graph as G

import SatO.Jasenrekisteri.Endpoints
import SatO.Jasenrekisteri.Markup
import SatO.Jasenrekisteri.Person
import SatO.Jasenrekisteri.Tag
import SatO.Jasenrekisteri.World

tagPage :: TagName -> QueryM (HtmlPage "tag")
tagPage tn = do
    world <- ask
    let tag = world ^. worldTags . att tn
    pure $ tagPage' world tag

-- TODO: use closure fields
tagPage' :: World -> Tag -> HtmlPage "tag"
tagPage' world tag = template' ("Tagi: " <> tn ^. _TagName) $ do
    subheader_ "Alatagit"
    tagList_ tags
    subheader_ "Jäsenet"
    memberList_ tags (world ^.. membersFold)
  where
    tn = tag ^. tagName

    -- | TODO: use tag closure
    tags :: [Tag]
    tags = world ^.. subtagsFold . filtered (\subtag -> subtag ^. tagName /= tn)

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
