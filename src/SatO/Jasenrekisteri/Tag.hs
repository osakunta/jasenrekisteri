{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module SatO.Jasenrekisteri.Tag (
    -- * Tag name
    TagName (..),
    _TagName,
    TagNames (..),
    _TagNames,
    tagNamesOf,
    tagNamesToText,
    -- * Full Tag
    Tag (..),
    tagName,
    tagColour,
    tagChildren,
    TagColour,
    TagHierarchy,
    _TagHierarchy,
    tagHierarchyOf,
    ifoldedTagHierarchy,
    ) where

import Control.Lens
import Control.Lens.Att
import Futurice.Generics
import Futurice.Generics.SOP  (sopToEncoding, sopToJSON)
import Futurice.IdMap         (HasKey (..))
import Futurice.Prelude
import Lucid                  (ToHtml (..))
import Network.HTTP.Types.URI (urlDecode)
import Prelude ()
import Web.HttpApiData        (FromHttpApiData (..), ToHttpApiData (..))

import           Data.Set.Lens  (setOf)
import           Futurice.Graph (Graph)
import qualified Futurice.Graph as Graph

import qualified Data.Aeson         as A
import qualified Data.Aeson.Types   as A
import qualified Data.Csv           as Csv
import qualified Data.Set           as Set
import qualified Data.Text          as T
import qualified Data.Text.Encoding as TE

------------------------------------------------------------------------------
-- TagName
------------------------------------------------------------------------------

newtype TagName = TagName { getTagName :: Text }
    deriving (Eq, Ord, Show, Read, Generic)

makePrisms ''TagName
makeWrapped ''TagName

instance IsString TagName where
    fromString = TagName . fromString

instance Hashable TagName

instance A.FromJSON TagName where
    parseJSON = fmap TagName . A.parseJSON

instance A.ToJSON TagName where
    toJSON = A.toJSON . getTagName

instance A.ToJSONKey TagName where
    toJSONKey = A.toJSONKeyText getTagName

instance A.FromJSONKey TagName where
    fromJSONKey = A.fromJSONKeyCoerce

instance ToHtml TagName where
    toHtmlRaw = toHtmlRaw . getTagName
    toHtml = toHtml . getTagName

instance ToHttpApiData TagName where
    toUrlPiece = getTagName

instance FromHttpApiData TagName where
    parseUrlPiece = pure . TagName . decodeUtf8Lenient . urlDecode False . encodeUtf8

instance Arbitrary TagName where
    arbitrary = TagName <$> arbitrary

------------------------------------------------------------------------------
-- TagNames
------------------------------------------------------------------------------

newtype TagNames = TagNames { getTagNames :: Set TagName }
    deriving (Eq, Ord, Show, Read, Generic)

makePrisms ''TagNames

-- |
--
-- @
-- tagNamesOf :: Fold s TagName       -> s -> TagNames
-- @
tagNamesOf :: Getting _ s TagName -> s -> TagNames
tagNamesOf l s = TagNames $ setOf (l . filtered (/= TagName "")) s

tagNamesToText :: TagNames -> Text
tagNamesToText = T.intercalate "," . map getTagName . Set.toList . getTagNames

instance Semigroup TagNames where
    TagNames a <> TagNames b = TagNames (a <> b)

instance Monoid TagNames where
    mempty = TagNames mempty
    mappend = (<>)

instance Csv.FromField TagNames where
    parseField bs = pure $ tagNamesOf folded ts
      where
        ts = TagName . T.strip <$> T.splitOn "," (TE.decodeUtf8 bs)

instance Csv.ToField TagNames where
    toField = TE.encodeUtf8 . tagNamesToText

instance A.FromJSON TagNames where
    parseJSON = fmap (tagNamesOf folded) . (A.parseJSON :: A.Value -> A.Parser [TagName])

instance A.ToJSON TagNames where
    toJSON = A.toJSON . getTagNames

type instance Index TagNames = TagName
type instance IxValue TagNames = ()

instance Ixed TagNames where
    ix i = _TagNames . ix i

instance Contains TagNames where
    contains i = _TagNames . contains i

------------------------------------------------------------------------------
-- TagColour
------------------------------------------------------------------------------

type TagColour = Int

-------------------------------------------------------------------------------
-- Tag
-------------------------------------------------------------------------------

data Tag = Tag
    { _tagName     :: !TagName
    , _tagColour   :: !TagColour
    , _tagChildren :: !TagNames
    }
  deriving
    (Eq, Ord, Show, Generic, Typeable)

deriveGeneric ''Tag
makeLenses ''Tag

instance HasKey Tag where
    type Key Tag = TagName
    key = tagName

instance Graph.IsNode Tag where
    nodeNeighbors = toList . getTagNames . _tagChildren

instance A.ToJSON Tag where
    toJSON = sopToJSON
    toEncoding = sopToEncoding

instance A.FromJSON Tag where
    parseJSON = A.withObject "Tag" $ \obj -> Tag
        <$> obj A..: "name"
        <*> obj A..:? "colour" A..!= 0
        <*> obj A..:? "children" A..!= mempty

-------------------------------------------------------------------------------
-- TagHierarchy
-------------------------------------------------------------------------------

-- | Tag hierachy.
--
--
newtype TagHierarchy = TagHierarchy { getTagHierarcy :: Graph Tag }

makePrisms ''TagHierarchy

ifoldedTagHierarchy :: IndexedFold TagName TagHierarchy Tag
ifoldedTagHierarchy = to (Graph.toMap . getTagHierarcy) . ifolded

-- |
--
-- @
-- tagHierarchyOf :: Fold s Tag       -> s -> TagHierarchy
-- @
tagHierarchyOf :: Getting _ s Tag -> s -> TagHierarchy
tagHierarchyOf g s = TagHierarchy . Graph.fromList $ toListOf g s

instance ToJSON TagHierarchy where
    toJSON = toJSON . Graph.toMap . getTagHierarcy

type instance Index TagHierarchy = TagName
type instance IxValue TagHierarchy = Tag

instance Semigroup TagHierarchy where
    TagHierarchy a <> TagHierarchy b = TagHierarchy (a <> b)

instance Ixed TagHierarchy where
    ix = att

instance Att TagHierarchy where
    att name = lens getter' setter
      where
        emptyTag = Tag name 0 mempty
        getter' (TagHierarchy g) = fromMaybe emptyTag $ Graph.lookup name g
        setter (TagHierarchy g) tag = TagHierarchy $ Graph.insert tag g
{-
We could strip empty flags out, but then we couldn't find them while iterating.

            | tag == emptyTag = TagHierarchy $ Graph.insert tag g
            | otherwise       = TagHierarchy $ Graph.deleteKey name g
-}
