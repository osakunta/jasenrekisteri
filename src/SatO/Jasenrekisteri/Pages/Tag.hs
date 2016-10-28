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
    world <- ask
    let tag = world ^. worldTags . att tn
    pure $ tagPage' lu world tag

-- TODO: use closure fields
tagPage' :: LoginUser -> World -> Tag -> HtmlPage "tag"
tagPage' lu world tag = template' lu ("Tagi: " <> tn ^. _TagName) $ do
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
