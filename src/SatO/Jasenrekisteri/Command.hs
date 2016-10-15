{-# LANGUAGE OverloadedStrings #-}
module SatO.Jasenrekisteri.Command where

import Control.Lens
import Data.Aeson
import Futurice.Prelude
import Prelude ()

import SatO.Jasenrekisteri.Person
import SatO.Jasenrekisteri.Tag
import SatO.Jasenrekisteri.World

data Command
    = CmdAddTag PersonId TagName
    | CmdRemoveTag PersonId TagName
  deriving (Eq, Show)

instance FromJSON Command where
    parseJSON = withObject "Command" $ \obj -> do
        cmd <- obj .: "type"
        case (cmd :: Text) of
          "add-tag"    -> CmdAddTag <$> obj .: "memberId" <*> obj .: "tagName"
          "remove-tag" -> CmdRemoveTag <$> obj .: "memberId" <*> obj .: "tagName"
          _            -> fail $ "Unknown command: " <> cmd ^. from packed

applyCommand :: Command -> World -> World
applyCommand (CmdAddTag pid tn) w =
    w & worldMembers . ix pid . personTags . contains tn .~ True
applyCommand (CmdRemoveTag pid tn) w =
    w & worldMembers . ix pid . personTags . contains tn .~ False
