{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module SatO.Jasenrekisteri.Command where

import Prelude ()
import Futurice.Prelude
import Control.Lens         hiding ((.=))
import Data.Aeson
import Data.Aeson.Encoding  (pair)
import Data.Functor.Classes
       (Eq1 (..), Show1 (..), eq1, showsBinaryWith, showsUnaryWith)
import Data.Monoid          (mconcat)
import Futurice.Generics
import Web.HttpApiData      (FromHttpApiData (..), ToHttpApiData (..))

import SatO.Jasenrekisteri.Member
import SatO.Jasenrekisteri.MemberEdit
import SatO.Jasenrekisteri.Tag
import SatO.Jasenrekisteri.World

import qualified Database.PostgreSQL.Simple.FromField as P
import qualified Database.PostgreSQL.Simple.ToField   as P

data Command f
    = CmdAddTag MemberId TagName
    | CmdRemoveTag MemberId TagName
    | CmdEditMember MemberId MemberEdit
    | CmdNewMember (f MemberId) MemberEdit
--  deriving (Eq, Show)

traverseCommand :: Applicative m => (f MemberId -> m (g MemberId)) -> Command f -> m (Command g)
traverseCommand _f (CmdAddTag memberId tn)       = pure $ CmdAddTag memberId tn
traverseCommand _f (CmdRemoveTag memberId tn)    = pure $ CmdRemoveTag memberId tn
traverseCommand _f (CmdEditMember memberId edit) = pure $ CmdEditMember memberId edit
traverseCommand  f (CmdNewMember memberId tn)    = CmdNewMember <$> f memberId <*> pure tn

instance Eq1 f => Eq (Command f) where
    CmdAddTag a b     == CmdAddTag a' b'     = a == a'  && b == b'
    CmdRemoveTag a b  == CmdRemoveTag a' b'  = a == a'  && b == b'
    CmdEditMember a b == CmdEditMember a' b' = a == a'  && b == b'
    CmdNewMember a b  == CmdNewMember a' b'  = eq1 a a' && b == b'
    _ == _ = False

instance Show1 f => Show (Command f) where
    showsPrec d (CmdAddTag memberId tn) = showParen (d > 10)
        $ showString "CmdAddTag "
        . showsPrec 11 memberId
        . showString " "
        . showsPrec 11 (getTagName tn)
    showsPrec d (CmdRemoveTag memberId tn) = showParen (d > 10)
        $ showString "CmdRemoveTag "
        . showsPrec 11 memberId
        . showString " "
        . showsPrec 11 (getTagName tn)
    showsPrec d (CmdEditMember memberId edit) = showParen (d > 10)
        $ showString "CmdEditMember "
        . showsPrec 11 memberId
        . showString " "
        . showsPrec 11 edit
    showsPrec d (CmdNewMember memberId edit) =
        showsBinaryWith (liftShowsPrec showsPrec showList) showsPrec
        "CmdNewMember" d memberId edit

instance Show1 I where
    liftShowsPrec sp _ d (I x) =
        showsUnaryWith sp "I" d x

instance FromJSON1 Proxy where
    liftParseJSON _ _ Null = pure Proxy
    liftParseJSON _ _ _    = fail "Proxy should be encoded as Null"

commandMemberId :: Getter (Command I) MemberId
commandMemberId = to $ \cmd -> case cmd of
    CmdAddTag memberId _        -> memberId
    CmdRemoveTag memberId _     -> memberId
    CmdEditMember memberId _    -> memberId
    CmdNewMember (I memberId) _ -> memberId

instance FromJSON1 f => FromJSON (Command f) where
    parseJSON = withObject "Command" $ \obj -> do
        cmd <- obj .: "type"
        case (cmd :: Text) of
          "add-tag"     -> CmdAddTag <$> obj .: "memberId" <*> obj .: "tagName"
          "remove-tag"  -> CmdRemoveTag <$> obj .: "memberId" <*> obj .: "tagName"
          "member-edit" -> CmdEditMember <$> obj .: "memberId" <*> obj .: "edit"
          "member-new"  -> CmdNewMember <$> (obj .: "memberId" >>= parseJSON1) <*> obj .: "edit"
          _             -> fail $ "Unknown command: " <> cmd ^. from packed

instance ToJSON1 f => ToJSON (Command f) where
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
    toJSON (CmdEditMember mid pe) = object
        [ "type"     .= ("member-edit" :: Text)
        , "memberId" .= mid
        , "edit"     .= pe
        ]
    toJSON (CmdNewMember mid pe) = object
        [ "type"     .= ("member-new" :: Text)
        , "edit"     .= pe
        , "memberId" .= toJSON1 mid
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
    toEncoding (CmdEditMember mid pe) = pairs $ mconcat
        [ "type"     .= ("member-edit" :: Text)
        , "memberId" .= mid
        , "edit"     .= pe
        ]
    toEncoding (CmdNewMember mid pe) = pairs $ mconcat
        [ "type"     .= ("member-new" :: Text)
        , "edit"     .= pe
        ] <> pair "memberId" (toEncoding1 mid)

instance Arbitrary (f MemberId) => Arbitrary (Command f) where
    arbitrary = sopArbitrary

applyCommand :: Command I -> World -> World
applyCommand (CmdAddTag pid tn) w =
    w & worldMembers . ix pid . memberTags . contains tn .~ True
applyCommand (CmdRemoveTag pid tn) w =
    w & worldMembers . ix pid . memberTags . contains tn .~ False
applyCommand (CmdEditMember pid pe) w =
    w & worldMembers . ix pid %~ addMagicTags . toEndo pe
applyCommand (CmdNewMember (I pid) pe) w =
    w & worldMembers . at pid ?~ (addMagicTags $ toEndo pe $ emptyMember pid)

deriveGeneric ''Command

instance ToJSON1 f => P.ToField (Command f) where
    toField = P.toField . encode

instance (FromJSON1 f, Typeable f) => P.FromField (Command f) where
    fromField f mdata = do
        bs <- P.fromField f mdata
        case eitherDecode bs of
            Right x  -> return x
            Left err -> P.returnError P.ConversionFailed f err

newtype CID = CID Int64

instance P.ToField CID where
    toField (CID cid) = P.toField cid

instance P.FromField CID where
    fromField f mdata = CID <$> P.fromField f mdata

instance ToHttpApiData CID where
    toUrlPiece (CID cid) = toUrlPiece cid

instance FromHttpApiData CID where
    parseUrlPiece x = CID <$> parseUrlPiece x
