{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module SatO.Jasenrekisteri.Pages.Logout (logoutPage) where

import Futurice.Prelude
import Prelude ()
import SatO.Jasenrekisteri.Markup

logoutPage :: HtmlPage "logout"
logoutPage = template "Jäsenrekisteri" nav $ do
    row_ $ large_ 12 $ div_ [ class_ "callout success" ] $ do
        "Kirjauduttu ulos"
  where
    nav = div_ [ class_ "top-bar" ] $ do
        div_ [ class_ "top-bar-left" ] $ ul_ [ class_ "dropdown menu" ] $ do
            li_ [ class_ "menu-text"] $ do
                "Jäsenrekisteri"
                sup_ "2"

