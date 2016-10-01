{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Jasenrekisteri.Pages.Members (membersPage) where

import Futurice.Prelude
import Prelude ()

import Control.Lens
import Data.Ord     (comparing)
import Lucid

import qualified Data.Text   as T
import qualified Data.Vector as V

import Jasenrekisteri.HtmlUtils
import Jasenrekisteri.Person
import Jasenrekisteri.Context
import Jasenrekisteri.Types

membersPage :: JasenContext -> Html ()
membersPage ctx = template' "Jäsenet" $ do
    h2_ "Jäsenet"
    p_ $ toHtml $ "Yhteensä: " <> T.pack (show $ V.length members)
    ul_ [class_ "members"] $ foldMapOf folded (uncurry memberHtml) members'
  where
    members = ctx ^. ctxMembers

    members' :: [(UserId, Person)]
    members'
        = sortBy (comparing (_personFirstNames . snd) <> comparing (_personLastName . snd))
        . zip (map UserId [0..])
        . toList
        $ members

memberHtml :: UserId -> Person -> Html ()
memberHtml (UserId i) Person{..} =
    li_ $ a_ [href_ linkHref] $ do
        span_ [class_ "etu"] $ toHtml _personFirstNames
        " "
        span_ [class_ "suku"] $ toHtml _personLastName
  where
    linkHref = "/member/" <> T.pack (show i)
