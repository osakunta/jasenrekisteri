{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}
module SatO.Jasenrekisteri.Pages.Member (memberPage) where

import Prelude ()
import Futurice.Prelude
import Control.Lens         ((<&>))
import Control.Monad.Reader (ask)
import Futurice.IdMap       (HasKey (..))

import qualified Data.UUID    as UUID
import qualified Generics.SOP as SOP

import SatO.Jasenrekisteri.API
import SatO.Jasenrekisteri.Endpoints
import SatO.Jasenrekisteri.Markup
import SatO.Jasenrekisteri.Member
import SatO.Jasenrekisteri.MemberEdit
import SatO.Jasenrekisteri.Session
import SatO.Jasenrekisteri.Tag
import SatO.Jasenrekisteri.World

memberPage :: LoginUser -> MemberId -> QueryM (HtmlPage "member")
memberPage lu memberId = ask <&> \(world, today) -> case world ^? worldMembers . ix memberId of
    -- TODO: not found page
    Nothing -> page404 today lu
    Just p@Member {..} -> template' today lu (p ^. memberFullNameHtml) $ do
        let pid = p ^. key

        row_ $ large_ 12 $ dl_ $ do
            dt_ "Sähköposti"
            dd_ $ a_ [href_ $ "mailto:" <> _memberEmail] $ toHtml _memberEmail
            dt_ "Puhelin"
            dd_ $ a_ [href_ $ "tel:" <> _memberPhone] $ toHtml _memberPhone
            dt_ "Osoite"
            dd_ $ do
                toHtml _memberAddress
                br_ []
                toHtml $ _memberZipcode <> " " <> _memberCity

        subheader_ "Tagit"
        row_ $ large_ 12 $ div_ [ class_ "callout" ] $ do
            tagnameList_ world (world ^.. worldMemberTags . ix pid . _TagNames . folded)
            hr_ []
            row_ [ data_ "jrek-member-tag" $ UUID.toText pid ] $ do
                large_ 6 $ input_ [ type_ "text", placeholder_ "tagi" ]
                large_ 6 $ div_ [ class_ "button-group" ] $ do
                    button_ [ data_ "jrek-action" "add", class_ "button" ] "Lisää"
                    button_ [ data_ "jrek-action" "remove", class_ "button alert" ] "Poista"

        subheader_ "Muokkaa"
        row_ $ large_ 12 $ a_ [ memberlogHref pid ] "Muutosloki"
        hr_ []
        row_ $ large_ 12 $ div_ [ class_ "callout" ] $ div_ [ data_ "jrek-member-edit" $ UUID.toText pid] $ do
            for_ (SOP.hcollapse memberEdits) $ editbox p

            hr_ []
            div_ [ class_ "button-group" ] $ do
                button_ [ data_ "jrek-action" "submit", class_ "button" ] "Tallenna"

-------------------------------------------------------------------------------
-- Editbox
-------------------------------------------------------------------------------

editbox :: Member -> PE -> Html ()
editbox p (MkPE i l getter _) = label_ $ do
    toHtml l
    input_
        [ type_ "text"
        , data_ "jrek-field-name" i
        , data_ "jrek-field-value" $ p ^. getter
        , value_ $ p ^. getter
        ]
