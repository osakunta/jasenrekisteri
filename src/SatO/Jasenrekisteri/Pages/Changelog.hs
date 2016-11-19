{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}
module SatO.Jasenrekisteri.Pages.Changelog (changelogPage, memberlogPage) where

import Prelude ()
import Futurice.Prelude
import Data.Maybe       (fromMaybe, mapMaybe)

import qualified Data.Text as T
import qualified Data.UUID as UUID

import SatO.Jasenrekisteri.API
import SatO.Jasenrekisteri.Command
import SatO.Jasenrekisteri.Markup
import SatO.Jasenrekisteri.Person
import SatO.Jasenrekisteri.PersonEdit
import SatO.Jasenrekisteri.Session
import SatO.Jasenrekisteri.World

changelogPage :: LoginUser -> [(LoginUser, UTCTime, Command)] -> World -> HtmlPage "changelog"
changelogPage lu cmds world = template' lu "Muutosloki" $ do
    row_ $ large_ 12 $ table_ $ do
        thead_ $ tr_ $ do
            th_ "Muokkaaja"
            th_ "Aika"
            th_ "Jäsen"
            th_ "Muutos"
        tbody_ $ for_ cmds $ \(editor, stamp, cmd) -> tr_ $ do
            let memberId = cmd ^. commandMemberId
            td_ $ toHtml editor
            td_ $ toHtml $ show $ utcToHelsinkiTime stamp
            td_ $ a_ [ memberlogHref memberId ] $ toHtml $ fromMaybe "<tuntematon>" $
                world ^? worldMembers . ix memberId . personFullName
            td_ $ case cmd of
                    CmdAddTag _ tn -> do
                        span_ [ class_ "jrek-added" ] "Lisätty"
                        " "
                        tagNameLink_ world tn
                    CmdRemoveTag _ tn -> do
                        span_ [ class_ "jrek-removed" ] "Poistettu"
                        " "
                        tagNameLink_ world tn
                    CmdEditPerson _ pe -> toHtml $
                        "Muokattu: " <> T.intercalate ", " (mapMaybe (f pe) personEdits')
  where
    f pe (MkPE _ ftitle _ e) = ftitle <$ pe ^. e

memberlogPage
    :: LoginUser                        -- ^ Viewer
    -> PersonId                         -- ^ Person's memberlog
    -> World                            -- ^ Original world, needed to show "from" states.
    -> World                            -- ^ Current world
    -> [(LoginUser, UTCTime, Command)]  -- ^ Changes
    -> HtmlPage "memberlog"
memberlogPage lu memberId origWorld world cmds =
    template' lu ("Muutosloki - " <> name <> " - " <> UUID.toText memberId) $ do
        row_ $ large_ 12 $ a_ [ memberHref memberId ] $ "Jäsenen sivu"
        hr_ []
        row_ $ large_ 12 $ table_ $ do
            thead_ $ tr_ $ do
                th_ "Muokkaaja"
                th_ "Aika"
                th_ "Muutos"
            tbody_ $ for_ (members origMember cmds) $ \(currMember, editor, stamp, cmd) -> tr_ $ do
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
                    CmdEditPerson _ pe -> dl_ $ for_ personEdits' $ \(MkPE _ ftitle fp fpe) ->
                        case pe ^. fpe of
                            Nothing  -> pure ()
                            Just new -> do
                                dt_ $ toHtml ftitle
                                dd_ $ do
                                    span_ [ class_ "jrek-removed" ] $ toHtml (ifEmpty "<tyhjä>" $ currMember ^. fp)
                                    " → "
                                    span_ [ class_ "jrek-added" ] $ toHtml new

  where
    member = fromMaybe (emptyPerson memberId) $ world ^? worldMembers . ix memberId
    origMember = fromMaybe (emptyPerson memberId) $ origWorld ^? worldMembers . ix memberId
    name = ifEmpty "<tuntematon>" $ member ^. personFullName

ifEmpty :: Text -> Text -> Text
ifEmpty def t
    | T.null t  = def
    | otherwise = t

members
    :: Person
    -> [(LoginUser, UTCTime, Command)]
    -> [(Person, LoginUser, UTCTime, Command)]
members = scan f
  where
    f member (lu, stamp, command) = ((member, lu, stamp, command), member')
      where
        member' = case command of
            CmdEditPerson _ pe -> toEndo pe member
            _                  -> member

scan :: (s -> a -> (b, s)) -> s -> [a] -> [b]
scan _f _initial []       = []
scan  f  initial (a : as) = case f initial a of
    ~(b, s) -> b : scan f s as
