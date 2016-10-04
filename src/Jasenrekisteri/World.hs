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
--    worldTagPersons,
    -- * Misc
    personHasTag,
    ) where

import Control.Lens
import Futurice.Prelude
import Prelude ()

import           Futurice.IdMap (IdMap)
import qualified Futurice.IdMap as IdMap
import qualified Futurice.Graph as G

import Jasenrekisteri.Person
import Jasenrekisteri.Tag

-- | 'World' contains all the data we display.
--
data World = World
    { _worldMembers :: !(IdMap Person)
    , _worldTags    :: !TagHierarchy

    -- Lazy fields, constructed on need:
    --
    , _worldTagClosures :: Map TagName TagNames
    , _worldRevTagClosures :: Map TagName TagNames

    , _worldPersonTags  :: Map PersonId TagNames
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
mkWorld persons tags = mkWorld' persons' (tags' <> personTags' <> childTags)
  where
    persons'    = IdMap.idMapOf folded persons
    tags'       = tagHierarchyOf folded tags
    personTags' = tagHierarchyOf
        (folded . personTags . _TagNames . folded . to toTag)
        persons
    -- children of specified tags
    childTags   = tagHierarchyOf
        (folded . tagChildren . _TagNames . folded . to toTag)
        tags

    toTag :: TagName -> Tag
    toTag tn = Tag tn 0 mempty

mkWorld' :: IdMap Person -> TagHierarchy -> World
mkWorld' persons tags = World
    { _worldMembers = persons
    , _worldTags    = tags
    -- Lazy fields
    , _worldTagClosures = tagClosures
    , _worldRevTagClosures = revTagClosures
    , _worldPersonTags = personTagsClosure
    }
  where
    tagClosures = toMapOf (ifoldedTagHierarchy . to tagClosure) tags
    tagClosure tag = tagNamesOf
        (_Just . folded . tagName)
        $ G.closure (tags ^. _TagHierarchy) [tag ^. tagName]

    revTagClosures = toMapOf (ifoldedTagHierarchy . to revTagClosure) tags
    revTagClosure tag = tagNamesOf
        (_Just . folded . tagName)
        $ G.revClosure (tags ^. _TagHierarchy) [tag ^. tagName]

    personTagsClosure :: Map PersonId TagNames
    personTagsClosure = toMapOf (IdMap.ifolded . to personTag) persons

    personTag :: Person -> TagNames
    personTag person = person ^.
        (personTags . _TagNames . folded. to (\tn -> revTagClosures ^? ix tn) . _Just)


-------------------------------------------------------------------------------
-- Lenses
-------------------------------------------------------------------------------

worldMembers :: Lens' World (IdMap Person)
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
worldPersonTags :: (Profunctor p, Contravariant f) => Optic' p f World (Map PersonId TagNames)
worldPersonTags = to _worldPersonTags

{-
-- |
--
-- @
-- worldTagPersons :: Getter World (Map TagName [Person])
-- @
worldTagPersons :: (Profunctor p, Contravariant f) => Optic' p f World (Map TagName [Person])
worldTagPersons = to _worldTagPersons
-}

-------------------------------------------------------------------------------
-- Qiery
-------------------------------------------------------------------------------

-- TODO:
personHasTag :: World -> Person -> TagName -> Bool
personHasTag _world _person _tag = False
