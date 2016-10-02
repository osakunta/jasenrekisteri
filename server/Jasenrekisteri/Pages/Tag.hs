{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Jasenrekisteri.Pages.Tag (tagPage) where

import Futurice.Prelude
import Prelude ()

import Lucid hiding (for_)

{-
import Jasenrekisteri.HtmlUtils
import Jasenrekisteri.Person
-}
import Jasenrekisteri.Tag
import Jasenrekisteri.World

tagPage :: World -> TagName -> Html ()
tagPage _ _ = pure () 
