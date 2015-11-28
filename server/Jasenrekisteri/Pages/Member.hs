{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Jasenrekisteri.Pages.Member (memberPage) where

import Prelude        ()
import Prelude.Compat

import Data.Foldable (traverse_)
import Data.Monoid   ((<>))
import Lucid

import Jasenrekisteri.HtmlUtils
import Jasenrekisteri.Person
import Jasenrekisteri.Tag

memberPage :: Person -> Html ()
memberPage Person{..} = template' name $ do
    dl_ $ do
        dt_ "Sähköposti"
        dd_ $ a_ [href_ $ "mailto:" <> personEmail] $ toHtml personEmail
        dt_ "Puhelin"
        dd_ $ a_ [href_ $ "tel:" <> personPhone] $ toHtml personPhone
        dt_ "Osoite"
        dd_ $ do
            toHtml personAddress
            br_ []
            toHtml $ personZipcode <> " " <> personCity
    h2_ "Tagit"
    ul_ [class_ "tags"] $
        traverse_ (li_ . renderTag) $ getTags personTags
  where
    name = personFirstNames <> " " <> personLastName
