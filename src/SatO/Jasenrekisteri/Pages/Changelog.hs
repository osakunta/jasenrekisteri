{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}
module SatO.Jasenrekisteri.Pages.Changelog (changelogPage, memberlogPage) where

import Prelude ()
import Futurice.Prelude
import Data.Maybe       (fromMaybe, mapMaybe)
import Data.Time        (defaultTimeLocale, formatTime)

import qualified Data.Text as T
import qualified Data.UUID as UUID

import SatO.Jasenrekisteri.API
import SatO.Jasenrekisteri.Command
import SatO.Jasenrekisteri.Markup
import SatO.Jasenrekisteri.Member
import SatO.Jasenrekisteri.MemberEdit
import SatO.Jasenrekisteri.Session
import SatO.Jasenrekisteri.World

changelogPage :: GoogleClientId -> Day -> LoginUser -> [(CID, LoginUser, UTCTime, Command I)] -> World -> HtmlPage "changelog"
changelogPage gcid today lu cmds world = template' gcid today lu "Muutosloki" $ do
    row_ $ large_ 12 $ table_ $ do
        thead_ $ tr_ $ do
            th_ "Muokkaaja"
            th_ "Aika"
            th_ "Jäsen"
            th_ "Muutos"
        tbody_ $ for_ cmds $ \(_, editor, stamp, cmd) -> tr_ $ do
            let memberId = cmd ^. commandMemberId
            td_ $ toHtml editor
            td_ $ toHtml $ formatTime defaultTimeLocale "%F %H:%m" $ utcToHelsinkiTime stamp
            td_ $ a_ [ memberlogHref memberId ] $ fromMaybe "<tuntematon>" $
                world ^? worldMembers . ix memberId . memberFullNameHtml
            td_ $ case cmd of
                    CmdNewMember _ _ -> "Luotu"
                    CmdAddTag _ tn -> do
                        span_ [ class_ "jrek-added" ] "Lisätty"
                        " "
                        tagNameLink_ world tn
                    CmdRemoveTag _ tn -> do
                        span_ [ class_ "jrek-removed" ] "Poistettu"
                        " "
                        tagNameLink_ world tn
                    CmdEditMember _ pe -> toHtml $
                        "Muokattu: " <> T.intercalate ", " (mapMaybe (f pe) memberEdits')
    case safeLast cmds of
        Nothing -> pure ()
        Just (cid, _, _, _) -> do
            hr_ []
            row_ $ large_ 12 $ a_ [ changelogHref (Just cid) ] "Näytä vanhemmat"
  where
    f pe (MkPE _ ftitle _ e) = ftitle <$ pe ^. e

memberlogPage
    :: GoogleClientId
    -> Day
    -> LoginUser                        -- ^ Viewer
    -> MemberId                         -- ^ Member's memberlog
    -> World                            -- ^ Original world, needed to show "from" states.
    -> World                            -- ^ Current world
    -> [(LoginUser, UTCTime, Command I)]  -- ^ Changes
    -> HtmlPage "memberlog"
memberlogPage gcid today lu memberId origWorld world cmds =
    template' gcid today lu ("Muutosloki - " <> name <> " - " <> toHtml (UUID.toText memberId)) $ do
        row_ $ large_ 12 $ a_ [ memberHref memberId ] $ "Jäsenen sivu"
        hr_ []
        row_ $ large_ 12 $ table_ $ do
            thead_ $ tr_ $ do
                th_ "Muokkaaja"
                th_ "Aika"
                th_ "Muutos"
            tbody_ $ for_ (reverse $ members origMember $ reverse cmds) $ \(currMember, editor, stamp, cmd) -> tr_ $ do
                td_ $ toHtml editor
                td_ $ toHtml $ formatTime defaultTimeLocale "%F %H:%m" $ utcToHelsinkiTime stamp
                td_ $ case cmd of
                    CmdAddTag _ tn -> do
                        span_ [ class_ "jrek-added" ] "Lisätty"
                        " "
                        tagNameLink_ world tn
                    CmdRemoveTag _ tn -> do
                        span_ [ class_ "jrek-removed" ] "Poistettu"
                        " "
                        tagNameLink_ world tn
                    CmdNewMember _ pe  -> dl_ $ for_ memberEdits' $ \(MkPE _ ftitle _ fpe) ->
                        case pe ^. fpe of
                            Nothing  -> pure ()
                            Just new -> do
                                dt_ $ toHtml ftitle
                                dd_ $ span_ [ class_ "jrek-added" ] $ toHtml new
                    CmdEditMember _ pe -> dl_ $ for_ memberEdits' $ \(MkPE _ ftitle fp fpe) ->
                        case pe ^. fpe of
                            Nothing  -> pure ()
                            Just new -> do
                                dt_ $ toHtml ftitle
                                dd_ $ do
                                    span_ [ class_ "jrek-removed" ] $ toHtml (ifEmpty "<tyhjä>" $ currMember ^. fp)
                                    " → "
                                    span_ [ class_ "jrek-added" ] $ toHtml new
  where
    member = fromMaybe (emptyMember memberId) $ world ^? worldMembers . ix memberId
    origMember = fromMaybe (emptyMember memberId) $ origWorld ^? worldMembers . ix memberId

    name :: Html ()
    name = member ^. memberFullNameHtml

ifEmpty :: Text -> Text -> Text
ifEmpty def t
    | T.null t  = def
    | otherwise = t

members
    :: Member
    -> [(LoginUser, UTCTime, Command I)]
    -> [(Member, LoginUser, UTCTime, Command I)]
members = scan f
  where
    f member (lu, stamp, command) = ((member, lu, stamp, command), member')
      where
        member' = case command of
            CmdEditMember _ pe            -> toEndo pe member
            CmdNewMember (I memberId)  pe -> toEndo pe $ emptyMember memberId
            _                             -> member

scan :: (s -> a -> (b, s)) -> s -> [a] -> [b]
scan _f _initial []       = []
scan  f  initial (a : as) = case f initial a of
    ~(b, s) -> b : scan f s as

safeLast :: [a] -> Maybe a
safeLast []       = Nothing
safeLast [x]      = Just x
safeLast (_ : xs) = safeLast xs
