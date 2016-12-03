{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module SatO.Jasenrekisteri.World (
    -- * Creation
    World(..),
    mkWorld,
    -- * Lenses
    worldMembers,
    worldTags,
    -- * Getters
    worldMemberTags,
    worldTagMembers,
    worldTagMemberCount,
    ) where

import Prelude ()
import Futurice.Prelude
import Control.Lens
import Data.Set.Lens    (setOf)
import Futurice.IdMap   (IdMap)

import qualified Data.Set       as Set
import qualified Futurice.Graph as G
import qualified Futurice.IdMap as IdMap

import SatO.Jasenrekisteri.Member
import SatO.Jasenrekisteri.Tag

-- | 'World' contains all the data we display.
--
data World = World
    { _worldMembers :: !(IdMap Member)
    , _worldTags    :: !TagHierarchy

    -- Lazy fields, constructed on need:
    --
    , _worldTagClosures    :: Map TagName TagNames
    , _worldRevTagClosures :: Map TagName TagNames

    , _worldMemberTags     :: Map MemberId TagNames
    , _worldTagMembers     :: Map TagName (Set MemberId)
    , _worldTagMemberCount :: Map TagName (Sum Int)
    }

-------------------------------------------------------------------------------
-- Smart constructor
-------------------------------------------------------------------------------

-- TODO: remove cycles,  change to return diagnostics too
mkWorld
    :: (Foldable f, Foldable f')
    => f Member
    -> f' Tag
    -> World
mkWorld members tags = mkWorld' members' (tags' <> memberTags' <> childTags)
  where
    members'    = IdMap.idMapOf folded members
    tags'       = tagHierarchyOf folded tags
    memberTags' = tagHierarchyOf
        (folded . memberTags . _TagNames . folded . to toTag)
        members
    -- children of specified tags
    childTags   = tagHierarchyOf
        (folded . tagChildren . _TagNames . folded . to toTag)
        tags

    toTag :: TagName -> Tag
    toTag tn = Tag tn 0 mempty

mkWorld' :: IdMap Member -> TagHierarchy -> World
mkWorld' members tags = World
    { _worldMembers = members
    , _worldTags    = tags
    -- Lazy fields
    , _worldTagClosures    = tagClosures
    , _worldRevTagClosures = revTagClosures
    , _worldMemberTags     = memberTagsClosure
    , _worldTagMembers     = tagMembers
    , _worldTagMemberCount = tagMemberCounts
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

    memberTagsClosure :: Map MemberId TagNames
    memberTagsClosure = toMapOf (IdMap.ifolded . to memberTag) members

    memberTag :: Member -> TagNames
    memberTag member = member ^.
        (memberTags . _TagNames . folded. to (\tn -> revTagClosures ^? ix tn) . _Just)

    tagMembers :: Map TagName (Set MemberId)
    tagMembers = tagClosures <&> \cl -> setOf
        (folded . filtered (\p -> overlaps (p ^. memberTags) cl) . memberUuid)
        members

    tagMemberCounts :: Map TagName (Sum Int)
    tagMemberCounts = fmap (Sum . length) tagMembers

 {- tagClosures <&> \cl -> Sum $ sumOf
        (folded . memberTags . filtered (overlaps cl) . to (const 1))
        members -}

overlaps :: TagNames -> TagNames -> Bool
overlaps tns tns' = not $ null $ Set.intersection
    (tns ^. _TagNames) (tns' ^. _TagNames)

-------------------------------------------------------------------------------
-- Lenses
-------------------------------------------------------------------------------

worldMembers :: Lens' World (IdMap Member)
worldMembers = lens _worldMembers $ \world members ->
    mkWorld (members ^.. folded) (world ^.. worldTags . ifoldedTagHierarchy)

worldTags :: Lens' World TagHierarchy
worldTags = lens _worldTags $ \world tags ->
    mkWorld (world ^.. worldMembers . folded) (tags ^.. ifoldedTagHierarchy)

-- |
--
-- @
-- worldMemberTags :: Getter World (Map Memberid [Tag])
-- @
worldMemberTags :: (Profunctor p, Functor f, Contravariant f) => Optic' p f World (Map MemberId TagNames)
worldMemberTags = to _worldMemberTags

-- |
--
-- @
-- worldTagMemberCount :: Getter World (Map TagName (Sum Int))
-- @
worldTagMembers :: (Profunctor p, Functor f, Contravariant f) => Optic' p f World (Map TagName :$ Set MemberId)
worldTagMembers = to _worldTagMembers

-- |
--
-- @
-- worldTagMemberCount :: Getter World (Map TagName (Sum Int))
-- @
worldTagMemberCount :: (Profunctor p, Functor f, Contravariant f) => Optic' p f World (Map TagName :$ Sum Int)
worldTagMemberCount = to _worldTagMemberCount
