{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Jasenrekisteri.Pages.Members (membersPage) where

import Prelude        ()
import Prelude.Compat

import Control.Lens
import Data.List    (sortBy)
import Data.Monoid  ((<>))
import Data.Ord     (comparing)
import Data.Vector  (Vector)
import Lucid

import qualified Data.Text   as T
import qualified Data.Vector as V

import Jasenrekisteri.HtmlUtils
import Jasenrekisteri.Person
import Jasenrekisteri.Types

membersPage :: Vector Person -> Html ()
membersPage members = template' "Jäsenet" $ do
    h2_ "Jäsenet"
    p_ $ toHtml $ "Yhteensä: " <> T.pack (show $ V.length members)
    ul_ [class_ "members"] $ foldMapOf folded (uncurry memberHtml) members'
  where members' = V.fromList
                 . sortBy (comparing (personFirstNames . snd) <> comparing (personLastName . snd))
                 . zip (map UserId [0..])
                 . V.toList
                 $ members

memberHtml :: UserId -> Person -> Html ()
memberHtml (UserId i) Person{..} =
    li_ $ a_ [href_ linkHref] $ do
        span_ [class_ "etu"] $ toHtml personFirstNames
        " "
        span_ [class_ "suku"] $ toHtml personLastName
  where
    linkHref = "/member/" <> T.pack (show i)
