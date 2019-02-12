{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module SatO.Jasenrekisteri.Pages.Login (loginPage, logoutPage) where

import Prelude ()
import Futurice.Prelude
import SatO.Jasenrekisteri.Markup

loginPage :: GoogleClientId -> HtmlPage "login"
loginPage gcid = template gcid "Kirjaudu jäsenrekisteriin" nav $ do
    row_ $ large_ 12 $ 
        div_ [ class_ "g-signin2", data_ "onsuccess" "onSignIn" ] $ pure ()

logoutPage :: GoogleClientId -> HtmlPage "logout"
logoutPage gcid = template gcid "Kirjaudu ulos jäsenrekisterista" nav $ do
    row_ $ large_ 12 $ 
        a_ [ href_ "#", class_ "button", id_ "logout-link" ] "Kirjaudu ulos"

nav :: Monad m => HtmlT m ()
nav = div_ [ class_ "top-bar" ] $ do
    div_ [ class_ "top-bar-left" ] $ ul_ [ class_ "dropdown menu" ] $ do
        li_ [ class_ "menu-text"] $ do
            "Jäsenrekisteri"
            sup_ "2"
