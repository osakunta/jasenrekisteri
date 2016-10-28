{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}
module SatO.Jasenrekisteri.Pages.Member (memberPage) where

import Prelude ()
import Futurice.Prelude
import Control.Lens

import Control.Monad.Reader (ask)
import Futurice.IdMap       (HasKey (..))

import SatO.Jasenrekisteri.Endpoints
import SatO.Jasenrekisteri.Markup
import SatO.Jasenrekisteri.Person
import SatO.Jasenrekisteri.Session
import SatO.Jasenrekisteri.Tag
import SatO.Jasenrekisteri.World

memberPage :: LoginUser -> PersonId -> QueryM (HtmlPage "member")
memberPage lu personId = ask <&> \world -> case world ^? worldMembers . ix personId of
    -- TODO: not found page
    Nothing -> page404 lu
    Just p@Person {..} ->  template' lu (_personFirstNames <> " " <> _personLastName) $ do
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
