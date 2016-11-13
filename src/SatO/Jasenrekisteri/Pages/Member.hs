{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}
module SatO.Jasenrekisteri.Pages.Member (memberPage) where

import Prelude ()
import Futurice.Prelude
import Control.Lens (Getting, (<&>))
import Control.Monad.Reader (ask)
import Futurice.IdMap       (HasKey (..))

import qualified Generics.SOP as SOP

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
        row_ $ large_ 12 $ do
            tagnameList_ world (world ^.. worldPersonTags . ix (p ^. key) . _TagNames . folded)

        subheader_ "Muokkaa"
        row_ $ large_ 12 $ div_ [ class_ "callout" ] $ form_ $ do
            for_ (SOP.hcollapse personEditboxes) $ editbox p

            hr_ []
            div_ [ class_ "button-group" ] $ do
                button_ [ class_ "button" ] "Tallenna"
                button_ [ class_ "button warning" ] "Poista muutokset"

-------------------------------------------------------------------------------
-- Editbox
-------------------------------------------------------------------------------

data Editbox = Editbox Text Text (Getting Text Person Text)

editbox :: Person -> Editbox -> Html ()
editbox p (Editbox i l getter) = label_ $ do
    toHtml l
    input_ [ type_ "text", data_ "jrek-orig-value" i, value_ $ p ^. getter ]

personEditboxes :: NP (K Editbox) (UnSingleton (SOP.Code PersonEdit))
personEditboxes =
    K (Editbox "birthday" "Syntymäpäivä" personBirthday) :*
    K (Editbox "birthplace" "Syntymäpaikka" personBirthplace) :*
    K (Editbox "lastName" "Sukunimi" personLastName) :*
    K (Editbox "firstNames" "Etunimiet" personFirstNames) :*
    K (Editbox "matrikkeli" "Matrikkeli" personMatrikkeli) :*
    K (Editbox "affilitationDate" "Liittymispäivä" personAffiliationDate) :*
    K (Editbox "university" "Yliopisto" personUniversity) :*
    K (Editbox "tDK" "Tiedekunta" personTDK) :*
    K (Editbox "address" "Postiosoite" personAddress) :*
    K (Editbox "zipcode" "Postinumero" personZipcode) :*
    K (Editbox "city" "Kaupinki" personCity) :*
    K (Editbox "country" "Maa" personCountry) :*
    K (Editbox "email" "Sähköposti" personEmail) :*
    K (Editbox "phone" "Puhelinnumero" personPhone) :*
    Nil
