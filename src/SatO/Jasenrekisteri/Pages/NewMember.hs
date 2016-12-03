{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}
module SatO.Jasenrekisteri.Pages.NewMember (newMemberPage) where

import Prelude ()
import Futurice.Prelude

import qualified Generics.SOP as SOP

import SatO.Jasenrekisteri.Endpoints
import SatO.Jasenrekisteri.Markup
import SatO.Jasenrekisteri.MemberEdit
import SatO.Jasenrekisteri.Session

newMemberPage :: LoginUser -> QueryM (HtmlPage "new-member")
newMemberPage lu = do
    (_, today) <- ask
    pure $ template' today lu "Uusi jäsen" $ do
        row_ $ large_ 12 $ div_ [ class_ "callout" ] $ div_ [ data_ "jrek-member-new" ""] $ do
            for_ (SOP.hcollapse memberEdits) editbox

            hr_ []
            div_ [ class_ "button-group" ] $ do
                button_ [ data_ "jrek-action" "submit", class_ "button" ] "Luo"

editbox :: PE -> Html ()
editbox (MkPE i l _ _) = label_ $ do
    toHtml l
    input_
        [ type_ "text"
        , data_ "jrek-field-name" i
        , placeholder_ l
        ]
