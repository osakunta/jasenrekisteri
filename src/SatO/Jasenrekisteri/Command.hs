{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module SatO.Jasenrekisteri.Command where

import Prelude ()
import Futurice.Prelude
import Control.Lens      hiding ((.=))
import Data.Aeson
import Futurice.Generics

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

instance ToJSON Command where
    toJSON (CmdAddTag mid tn) = object
        [ "type"     .= ("add-tag" :: Text)
        , "memberId" .= mid
        , "tagName"  .= tn
        ]
    toJSON (CmdRemoveTag mid tn) = object
        [ "type"     .= ("remove-tag" :: Text)
        , "memberId" .= mid
        , "tagName"  .= tn
        ]

instance Arbitrary Command where
    arbitrary = sopArbitrary

applyCommand :: Command -> World -> World
applyCommand (CmdAddTag pid tn) w =
    w & worldMembers . ix pid . personTags . contains tn .~ True
applyCommand (CmdRemoveTag pid tn) w =
    w & worldMembers . ix pid . personTags . contains tn .~ False

deriveGeneric ''Command
