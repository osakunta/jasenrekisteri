{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}
module SatO.Jasenrekisteri.Pages.Admin (
    adminPage,
    dumpMembersCsv,
    dumpMembersXlsx,
) where

import Prelude ()
import Futurice.Prelude
import Control.Monad.Reader (ask)
import Futurice.IdMap       (HasKey (..))

import SatO.Jasenrekisteri.Endpoints
import SatO.Jasenrekisteri.Markup
import SatO.Jasenrekisteri.Session
import SatO.Jasenrekisteri.World

import SatO.Jasenrekisteri.DumpMember

adminPage :: LoginUser -> QueryM (HtmlPage "admin")
adminPage lu = do
    (world, today, gcid) <- ask
    pure $ adminPage' gcid today lu world

adminPage' :: GoogleClientId -> Day -> LoginUser -> World -> HtmlPage "admin"
adminPage' gcid today lu world = template' gcid today lu "Admin" $ do
    row_ $ large_ 12 $ do
        h2_ "Tilastot"

        p_ $ toHtml $ "Jäseniä: " <> show (length $ world ^.. worldMembers . folded)

        h2_ "Dump"

        ul_ $ do
            li_ $ a_ [ href_ "/admin/sato-jasenet.csv" ] "sato-jasenet.csv"
            li_ $ a_ [ href_ "/admin/sato-jasenet.xlsx" ] "sato-jasenet.xlsx"

-------------------------------------------------------------------------------
-- dump
-------------------------------------------------------------------------------

dumpMembersCsv :: LoginUser -> QueryM [DumpMember]
dumpMembersCsv _lu = do
    (world, _today, _gcid) <- ask
    pure
        [ dumpMember m (world ^. worldMemberTags . ix (m ^. key))
        | m <- world ^.. worldMembers . folded
        ]

dumpMembersXlsx :: LoginUser -> QueryM DumpMembers
dumpMembersXlsx lu = fmap DumpMembers (dumpMembersCsv lu)
