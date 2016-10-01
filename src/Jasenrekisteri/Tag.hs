{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module Jasenrekisteri.Tag (
    -- * Tag name
    TagName (..),
    TagNames (..),
    _TagNames,
    tagNamesOf,
    -- * Full Tag
    Tag (..),
    tagName,
    tagColour,
    tagChildren,
    TagColour,
    TagHierarchy (..),
    tagHierarchyOf,
    ) where

import Control.Lens
import Futurice.Generics
import Futurice.Prelude
import Prelude ()

import           Sato.Graph (Graph)
import qualified Sato.Graph as Graph

import Data.Set.Lens (setOf)

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

makeWrapped ''TagName

instance Hashable TagName

instance A.FromJSON TagName where
    parseJSON = fmap TagName . A.parseJSON

instance A.ToJSON TagName where
    toJSON = A.toJSON . getTagName

instance A.ToJSONKey TagName where
    toJSONKey = A.toJSONKeyText getTagName

instance A.FromJSONKey TagName where
    fromJSONKey = A.fromJSONKeyCoerce

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
    toField =
        TE.encodeUtf8 . T.intercalate "," . map getTagName . Set.toList . getTagNames

instance A.FromJSON TagNames where
    parseJSON = fmap (tagNamesOf folded) . (A.parseJSON :: A.Value -> A.Parser [TagName])

instance A.ToJSON TagNames where
    toJSON = A.toJSON . getTagNames

type instance Index TagNames = TagName
type instance IxValue TagNames = ()

instance Ixed TagNames where
    ix i = _TagNames . ix i

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

instance Graph.IsNode Tag where
    type Key Tag  = TagName
    nodeKey       = _tagName
    nodeNeighbors = toList . getTagNames . _tagChildren

instance A.ToJSON Tag where toJSON = sopToJSON
instance A.FromJSON Tag where parseJSON = sopParseJSON

-------------------------------------------------------------------------------
-- TagHierarchy
-------------------------------------------------------------------------------

newtype TagHierarchy = TagHierarchy { getTagHierarcy :: Graph Tag }

instance ToJSON TagHierarchy where
    toJSON = toJSON . Graph.toMap . getTagHierarcy

-- |
--
-- @
-- tagHierarchyOf :: Fold s Tag       -> s -> TagHierarchy
-- @
tagHierarchyOf :: Getting _ s Tag -> s -> TagHierarchy
tagHierarchyOf g s = TagHierarchy . Graph.fromList $ toListOf g s
