{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Jasenrekisteri.Tag (
    Tag(..),
    Tags(..),
    TagHierarchy(..),
    ) where

import Prelude        ()
import Prelude.Compat

import Data.Hashable         (Hashable)
import Data.HashMap.Strict   (HashMap)
import Data.Semigroup        (Semigroup (..))
import Data.Text             (Text)
import Data.Vector           (Vector)
import Data.Vector.Instances ()
import GHC.Generics          (Generic)

import qualified Data.Aeson.Extra   as A
import qualified Data.Csv           as Csv
import qualified Data.Text          as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector        as V

------------------------------------------------------------------------------
-- Tag
------------------------------------------------------------------------------

newtype Tag = Tag { getTag :: Text }
    deriving (Eq, Ord, Show, Read, Generic)

instance Hashable Tag

instance A.FromJSON Tag where
    parseJSON = fmap Tag . A.parseJSON

instance A.ToJSON Tag where
    toJSON = A.toJSON . getTag

instance A.FromJSONKey Tag where
    parseJSONKey = fmap Tag . A.parseJSONKey

instance A.ToJSONKey Tag where
    toJSONKey = A.toJSONKey . getTag

------------------------------------------------------------------------------
-- Tags
------------------------------------------------------------------------------

newtype Tags = Tags { getTags :: Vector Tag }
    deriving (Eq, Ord, Show, Read, Generic)

instance Semigroup Tags where
    Tags a <> Tags b = Tags (a <> b)

instance Monoid Tags where
    mempty = Tags mempty
    mappend = (<>)

instance Csv.FromField Tags where
    parseField = pure
               . Tags . V.fromList
               . map (Tag . T.strip)
               .  T.splitOn "," . TE.decodeUtf8

instance Csv.ToField Tags where
    toField =
        TE.encodeUtf8 . T.intercalate "," . map getTag . V.toList . getTags

instance A.FromJSON Tags where
    parseJSON = fmap Tags . A.parseJSON

instance A.ToJSON Tags where
    toJSON = A.toJSON . getTags

------------------------------------------------------------------------------
-- TagHierarchy
------------------------------------------------------------------------------

newtype TagHierarchy = TagHierarchy { getChildTags :: HashMap Tag Tags }
    deriving (Eq, Show, Read, Generic)

instance A.ToJSON TagHierarchy where
    toJSON = A.toJSON . A.M . getChildTags

instance A.FromJSON TagHierarchy where
    parseJSON = fmap (TagHierarchy . A.getMap) . A.parseJSON
