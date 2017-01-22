{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module SatO.Jasenrekisteri.Pages.Login (loginPage) where

import Prelude ()
import Futurice.Prelude
import SatO.Jasenrekisteri.Markup

loginPage :: GoogleClientId -> HtmlPage "login"
loginPage gcid = template gcid "Kirjaudu jäsenrekisteriin" nav $ do
    row_ $ large_ 12 $ 
        div_ [ class_ "g-signin2", data_ "onsuccess" "onSignIn" ] $ pure ()
  where
    nav = div_ [ class_ "top-bar" ] $ do
        div_ [ class_ "top-bar-left" ] $ ul_ [ class_ "dropdown menu" ] $ do
            li_ [ class_ "menu-text"] $ do
                "Jäsenrekisteri"
                sup_ "2"

