{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Jasenrekisteri.Pages.Members (membersPage) where

import Futurice.Prelude
import Prelude ()

import Control.Lens
import Data.Ord     (comparing)
import Lucid

import qualified Data.Text      as T
import qualified Futurice.IdMap as IdMap

import Jasenrekisteri.API
import Jasenrekisteri.HtmlUtils
import Jasenrekisteri.Person
import Jasenrekisteri.World

membersPage :: World -> Html ()
membersPage world = template' "Jäsenet" $ do
    h2_ "Jäsenet"
    p_ $ toHtml $ "Yhteensä: " <> T.pack (show $ length members)
    ul_ [class_ "members"] $ foldMapOf folded (uncurry memberHtml) members'
  where
    members = world ^. worldMembers

    members' :: [(PersonId, Person)]
    members'
        = sortBy (comparing (_personFirstNames . snd) <> comparing (_personLastName . snd))
        $ world ^@.. worldMembers . IdMap.ifolded

memberHtml :: PersonId -> Person -> Html ()
memberHtml i Person{..} =
    li_ $ a_ [memberHref i] $ do
        span_ [class_ "etu"] $ toHtml _personFirstNames
        " "
        span_ [class_ "suku"] $ toHtml _personLastName
