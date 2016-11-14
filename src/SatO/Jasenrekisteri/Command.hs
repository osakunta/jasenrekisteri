{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module SatO.Jasenrekisteri.Command where

import Prelude ()
import Futurice.Prelude
import Control.Lens      hiding ((.=))
import Data.Aeson
import Data.Monoid       (mconcat)
import Futurice.Generics

import SatO.Jasenrekisteri.Person
import SatO.Jasenrekisteri.PersonEdit
import SatO.Jasenrekisteri.Tag
import SatO.Jasenrekisteri.World

import qualified Database.PostgreSQL.Simple.FromField as P
import qualified Database.PostgreSQL.Simple.ToField   as P

data Command
    = CmdAddTag PersonId TagName
    | CmdRemoveTag PersonId TagName
    | CmdEditPerson PersonId PersonEdit
  deriving (Eq, Show)

instance FromJSON Command where
    parseJSON = withObject "Command" $ \obj -> do
        cmd <- obj .: "type"
        case (cmd :: Text) of
          "add-tag"     -> CmdAddTag <$> obj .: "memberId" <*> obj .: "tagName"
          "remove-tag"  -> CmdRemoveTag <$> obj .: "memberId" <*> obj .: "tagName"
          "member-edit" -> CmdEditPerson <$> obj .: "memberId" <*> obj .: "edit"
          _             -> fail $ "Unknown command: " <> cmd ^. from packed

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
    toJSON (CmdEditPerson mid pe) = object
        [ "type"     .= ("member-edit" :: Text)
        , "memberId" .= mid
        , "edit"     .= pe
        ]

    toEncoding (CmdAddTag mid tn) = pairs $ mconcat
        [ "type"     .= ("add-tag" :: Text)
        , "memberId" .= mid
        , "tagName"  .= tn
        ]
    toEncoding (CmdRemoveTag mid tn) = pairs $ mconcat
        [ "type"     .= ("remove-tag" :: Text)
        , "memberId" .= mid
        , "tagName"  .= tn
        ]
    toEncoding (CmdEditPerson mid pe) = pairs $ mconcat
        [ "type"     .= ("member-edit" :: Text)
        , "memberId" .= mid
        , "edit"     .= pe
        ]

instance Arbitrary Command where
    arbitrary = sopArbitrary

applyCommand :: Command -> World -> World
applyCommand (CmdAddTag pid tn) w =
    w & worldMembers . ix pid . personTags . contains tn .~ True
applyCommand (CmdRemoveTag pid tn) w =
    w & worldMembers . ix pid . personTags . contains tn .~ False
applyCommand (CmdEditPerson pid pe) w =
    w & worldMembers . ix pid %~ addMagicTags . toEndo pe

deriveGeneric ''Command

instance P.ToField Command where
    toField = P.toField . encode

instance P.FromField Command where
    fromField f mdata = do
        bs <- P.fromField f mdata
        case eitherDecode bs of
            Right x  -> return x
            Left err -> P.returnError P.ConversionFailed f err
