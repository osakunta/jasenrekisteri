{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}
module SatO.Jasenrekisteri.Pages.Changelog (changelogPage) where

import Prelude ()
import Futurice.Prelude

import qualified Data.UUID    as UUID

import SatO.Jasenrekisteri.Markup
import SatO.Jasenrekisteri.Command
import SatO.Jasenrekisteri.Person
import SatO.Jasenrekisteri.PersonEdit
import SatO.Jasenrekisteri.Session
import SatO.Jasenrekisteri.World

changelogPage
    :: LoginUser                        -- ^ Viewer
    -> PersonId                         -- ^ Person's changelog
    -> World                            -- ^ Original world, needed to show "from" states.
    -> World                            -- ^ Current world 
    -> [(LoginUser, UTCTime, Command)]  -- ^ Changes
    -> HtmlPage "changelog"
changelogPage lu memberId _origWorld world cmds =
    template' lu ("Muutosloki - " <> name <> " - " <> UUID.toText memberId) $ do
        pure ()
        row_ $ large_ 12 $ table_ $ do
            thead_ $ tr_ $ do
                th_ "Muokkaaja"
                th_ "Aika"
                th_ "Muutos"
            tbody_ $ for_ cmds $ \(editor, stamp, cmd) -> tr_ $ do
                td_ $ toHtml editor
                td_ $ toHtml $ show $ utcToHelsinkiTime stamp
                td_ $ case cmd of
                    CmdAddTag _ tn -> do
                        span_ [ class_ "jrek-added" ] "Lisätty"
                        " "
                        tagNameLink_ world tn 
                    CmdRemoveTag _ tn -> do
                        span_ [ class_ "jrek-removed" ] "Poistettu"
                        " "
                        tagNameLink_ world tn 
                    CmdEditPerson _ pe -> dl_ $ for_ personEdits' $ \(MkPE _ ftitle _fp fpe) ->
                        case pe ^. fpe of
                            Nothing  -> pure ()
                            Just new -> do
                                dt_ $ toHtml ftitle
                                dd_ $ do
                                    " → "
                                    span_ [ class_ "jrek-added" ] $ toHtml new
            
  where
    member = fromMaybe (emptyPerson memberId) $ world ^? worldMembers . ix memberId
    name = case member ^. personFullName of
        "" -> "<tuntematon>"
        n  -> n
