{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Jasenrekisteri.Tag (
    Tag(..),
    Tags(..),
    tagsLength,
    TagHierarchy(..),
    TagColour,
    TagColours(..),
    TagData(..),
    tagHierarchy,
    tagColours,
    ) where

import Prelude        ()
import Prelude.Compat

import Control.Lens
import Data.Hashable       (Hashable)
import Data.HashMap.Strict (HashMap)
import Data.Semigroup      (Semigroup (..))
import Data.Set            (Set)
import Data.Text           (Text)
import GHC.Generics        (Generic)

import qualified Data.Aeson.Extra    as A
import qualified Data.Csv            as Csv
import qualified Data.HashMap.Strict as HM
import qualified Data.Set            as Set
import qualified Data.Text           as T
import qualified Data.Text.Encoding  as TE

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

newtype Tags = Tags { getTags :: Set Tag }
    deriving (Eq, Ord, Show, Read, Generic)

makeLenses ''Tags

tagsLength :: Tags -> Int
tagsLength = Set.size . getTags

instance Semigroup Tags where
    Tags a <> Tags b = Tags (a <> b)

instance Monoid Tags where
    mempty = Tags mempty
    mappend = (<>)

instance Csv.FromField Tags where
    parseField = pure
               . Tags . Set.delete (Tag "") . Set.fromList
               . map (Tag . T.strip)
               .  T.splitOn "," . TE.decodeUtf8

instance Csv.ToField Tags where
    toField =
        TE.encodeUtf8 . T.intercalate "," . map getTag . Set.toList . getTags

instance A.FromJSON Tags where
    parseJSON = fmap (Tags . Set.delete (Tag "")) . A.parseJSON

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

------------------------------------------------------------------------------
-- TagColours
------------------------------------------------------------------------------

type TagColour = Int

newtype TagColours = TagColours { getTagColours :: HashMap Tag TagColour }
    deriving (Eq, Show, Read, Generic)

------------------------------------------------------------------------------
-- TagData
------------------------------------------------------------------------------

newtype TagData = TagData { getTagData :: HashMap Tag (Tags, TagColour) }

data TagDatum = TagDatum
    { _tdName     :: Tag
    , _tdColour   :: TagColour
    , _tdChildren :: Tags
    }
    deriving (Eq, Show, Read, Generic)

instance A.FromJSON TagDatum where
    parseJSON = A.withObject "Tag data" $ \obj ->
        TagDatum <$> obj A..: "name"
                 <*> obj A..: "color"
                 <*> obj A..: "children"

instance A.FromJSON TagData where
    parseJSON = fmap mkTagData . A.parseJSON

mkTagData :: [TagDatum] -> TagData
mkTagData = TagData . HM.fromList . map toPair
  where toPair (TagDatum n c ts) = (n, (ts, c))

tagHierarchy :: TagData -> TagHierarchy
tagHierarchy = TagHierarchy . HM.map fst . getTagData

tagColours :: TagData -> TagColours
tagColours = TagColours . HM.map snd . getTagData
