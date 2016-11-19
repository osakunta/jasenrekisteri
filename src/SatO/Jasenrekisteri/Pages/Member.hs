{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}
module SatO.Jasenrekisteri.Pages.Member (memberPage) where

import Prelude ()
import Futurice.Prelude
import Control.Lens         (Getting, (<&>))
import Control.Monad.Reader (ask)
import Futurice.IdMap       (HasKey (..))

import qualified Data.UUID    as UUID
import qualified Generics.SOP as SOP

import SatO.Jasenrekisteri.API
import SatO.Jasenrekisteri.Endpoints
import SatO.Jasenrekisteri.Markup
import SatO.Jasenrekisteri.Person
import SatO.Jasenrekisteri.PersonEdit
import SatO.Jasenrekisteri.Session
import SatO.Jasenrekisteri.Tag
import SatO.Jasenrekisteri.World

memberPage :: LoginUser -> PersonId -> QueryM (HtmlPage "member")
memberPage lu personId = ask <&> \world -> case world ^? worldMembers . ix personId of
    -- TODO: not found page
    Nothing -> page404 lu
    Just p@Person {..} ->  template' lu (p ^. personFullNameHtml) $ do
        let pid = p ^. key

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
        row_ $ large_ 12 $ div_ [ class_ "callout" ] $ do
            tagnameList_ world (world ^.. worldPersonTags . ix pid . _TagNames . folded)
            hr_ []
            row_ [ data_ "jrek-person-tag" $ UUID.toText pid ] $ do
                large_ 6 $ input_ [ type_ "text", placeholder_ "tagi" ]
                large_ 6 $ div_ [ class_ "button-group" ] $ do
                    button_ [ data_ "jrek-action" "add", class_ "button" ] "Lisää"
                    button_ [ data_ "jrek-action" "remove", class_ "button alert" ] "Poista"

        subheader_ "Muokkaa"
        row_ $ large_ 12 $ a_ [ memberlogHref pid ] "Muutosloki"
        hr_ []
        row_ $ large_ 12 $ div_ [ class_ "callout" ] $ div_ [ data_ "jrek-member-edit" $ UUID.toText pid] $ do
            for_ (SOP.hcollapse personEditboxes) $ editbox p

            hr_ []
            div_ [ class_ "button-group" ] $ do
                button_ [ data_ "jrek-action" "submit", class_ "button" ] "Tallenna"

-------------------------------------------------------------------------------
-- Markup
-------------------------------------------------------------------------------

tagnameList_ :: World -> [TagName] -> Html ()
tagnameList_ world ts = traverse_ (tagNameLink_ world) ts

-------------------------------------------------------------------------------
-- Editbox
-------------------------------------------------------------------------------

data Editbox = Editbox Text Text (Getting Text Person Text)

editbox :: Person -> Editbox -> Html ()
editbox p (Editbox i l getter) = label_ $ do
    toHtml l
    input_
        [ type_ "text"
        , data_ "jrek-field-name" i
        , data_ "jrek-field-value" $ p ^. getter
        , value_ $ p ^. getter
        ]

personEditboxes :: NP (K Editbox) (UnSingleton (SOP.Code PersonEdit))
personEditboxes =
    K (Editbox "birthday" "Syntymäpäivä" personBirthday) :*
    K (Editbox "birthplace" "Syntymäpaikka" personBirthplace) :*
    K (Editbox "lastName" "Sukunimi" personLastName) :*
    K (Editbox "firstNames" "Etunimet" personFirstNames) :*
    K (Editbox "matrikkeli" "Matrikkeli" personMatrikkeli) :*
    K (Editbox "affiliationDate" "Liittymispäivä" personAffiliationDate) :*
    K (Editbox "university" "Yliopisto" personUniversity) :*
    K (Editbox "tDK" "Tiedekunta" personTDK) :*
    K (Editbox "address" "Postiosoite" personAddress) :*
    K (Editbox "zipcode" "Postinumero" personZipcode) :*
    K (Editbox "city" "Kaupinki" personCity) :*
    K (Editbox "country" "Maa" personCountry) :*
    K (Editbox "email" "Sähköposti" personEmail) :*
    K (Editbox "phone" "Puhelinnumero" personPhone) :*
    Nil
