{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Jasenrekisteri.Pages.Member (memberPage) where

import Futurice.Prelude
import Prelude ()

import Futurice.IdMap (HasKey (..))

import Jasenrekisteri.HtmlUtils
import Jasenrekisteri.Person
import Jasenrekisteri.Tag
import Jasenrekisteri.World

memberPage :: World -> PersonId -> Html ()
memberPage world personId = case world ^? worldMembers . ix personId of
    -- TODO: not found page
    Nothing -> pure ()
    Just p@Person {..} ->  template' (_personFirstNames <> " " <> _personLastName) $ do
        row_ $ large_ 12 $ dl_ $ do
            dt_ "Sähköposti"
            dd_ $ a_ [href_ $ "mailto:" <> _personEmail] $ toHtml _personEmail
            dt_ "Puhelin"
            dd_ $ a_ [href_ $ "tel:" <> _personPhone] $ toHtml _personPhone
            dt_ "Osoite"
            dd_ $ do
                toHtml _personAddress
                br_ []
                toHtml $ _personZipcode <> " " <> _personCity
        subheader_ "Tagit"
        tagnameList_ world (world ^.. worldPersonTags . ix (p ^. key) . _TagNames . folded)
