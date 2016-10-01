{-# LANGUAGE TemplateHaskell #-}
module Jasenrekisteri.World (
    -- * Creation
    World(..),
    mkWorld,
    -- * Lenses
    worldMembers,
    worldTags,
    -- * Getters
    worldPersonTags,
    worldTagPersons,
    -- * Misc
    personHasTag,
    ) where

import Control.Lens
import Futurice.Prelude
import Prelude ()

import Data.Vector.Lens (toVectorOf)

import Jasenrekisteri.Person
import Jasenrekisteri.Tag

-- | 'World' contains all the data we display.
--
data World = World
    { _worldMembers :: !(Vector Person)
    , _worldTags    :: !TagHierarchy

    -- Lazy fields, constructed on need:
    --
    , _worldPersonTags :: Map PersonId [Tag]
      -- ^ All flags person has, included children tags
    , _worldTagPersons :: Map TagName [Person]
      -- ^ All persons with the tag
    }

-------------------------------------------------------------------------------
-- Smart constructor
-------------------------------------------------------------------------------

-- TODO: remove cycles,  change to return diagnostics too
mkWorld
    :: (Foldable f, Foldable f')
    => f Person
    -> f' Tag
    -> World
mkWorld persons tags = mkWorld'
    (toVectorOf folded persons)
    (tagHierarchyOf folded tags)

mkWorld' :: Vector Person -> TagHierarchy -> World
mkWorld' persons tags = World
    { _worldMembers = persons
    , _worldTags    = tags
    -- TODO:
    , _worldPersonTags = mempty
    , _worldTagPersons = mempty
    }

-------------------------------------------------------------------------------
-- Lenses
-------------------------------------------------------------------------------

worldMembers :: Lens' World (Vector Person)
worldMembers = lens _worldMembers $ \world members ->
    mkWorld' members (world ^. worldTags)

worldTags :: Lens' World TagHierarchy
worldTags = lens _worldTags $ \world tags ->
    mkWorld' (world ^. worldMembers) tags

-- |
--
-- @
-- worldPersonTags :: Getter World (Map Personid [Tag])
-- @
worldPersonTags :: (Profunctor p, Contravariant f) => Optic' p f World (Map PersonId [Tag])
worldPersonTags = to _worldPersonTags

-- |
--
-- @
-- worldTagPersons :: Getter World (Map TagName [Person])
-- @
worldTagPersons :: (Profunctor p, Contravariant f) => Optic' p f World (Map TagName [Person])
worldTagPersons = to _worldTagPersons

-------------------------------------------------------------------------------
-- Qiery
-------------------------------------------------------------------------------

-- TODO:
personHasTag :: World -> Person -> TagName -> Bool
personHasTag _world _person _tag = False
