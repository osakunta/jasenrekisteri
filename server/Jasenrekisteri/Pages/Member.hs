{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Jasenrekisteri.Pages.Member (memberPage) where

import Futurice.Prelude
import Prelude ()

import Lucid hiding (for_)

import Jasenrekisteri.HtmlUtils
import Jasenrekisteri.Person
import Jasenrekisteri.Tag
import Jasenrekisteri.World

memberPage :: World -> PersonId -> Html ()
memberPage world personId = case world ^? worldMembers . ix personId of
    -- TODO: not found page
    Nothing -> pure ()
    Just p@Person {..} ->  template' (_personFirstNames <> " " <> _personLastName) $ do
        dl_ $ do
            dt_ "Sähköposti"
            dd_ $ a_ [href_ $ "mailto:" <> _personEmail] $ toHtml _personEmail
            dt_ "Puhelin"
            dd_ $ a_ [href_ $ "tel:" <> _personPhone] $ toHtml _personPhone
            dt_ "Osoite"
            dd_ $ do
                toHtml _personAddress
                br_ []
                toHtml $ _personZipcode <> " " <> _personCity
        h2_ "Tagit"
        ul_ [class_ "tags"] $
            for_ (p ^. personTags . _TagNames) (tagNameLink_ world)
