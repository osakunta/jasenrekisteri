{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module SatO.Jasenrekisteri.SearchData (
    SearchItem (..),
    SearchItemType (..),
    ) where

import Prelude ()
import Futurice.Prelude
import Data.Swagger      (NamedSchema (..))
import Futurice.Generics

data SearchItemType
    = SearchItemMember
    | SearchItemTag
  deriving (Eq, Ord, Show)

instance ToJSON SearchItemType where
    toJSON SearchItemMember = "member"
    toJSON SearchItemTag    = "tag"

instance ToSchema SearchItemType where
    declareNamedSchema _ = pure $ NamedSchema (Just "Search item type") mempty


data SearchItem = SearchItem
    { searchItemLabel :: !Text
    , searchItemValue :: !Text
    , searchItemType  :: !SearchItemType
    , searchItemHref  :: !Text
    }
  deriving (Eq, Ord, Show)

deriveGeneric ''SearchItem

instance ToJSON SearchItem where
    toJSON     = sopToJSON
    toEncoding = sopToEncoding

instance ToSchema SearchItem where
    declareNamedSchema = sopDeclareNamedSchema
