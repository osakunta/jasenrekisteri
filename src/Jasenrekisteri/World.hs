{-# LANGUAGE TemplateHaskell #-}
module Jasenrekisteri.World (
    -- * Creation
    World(..),
    mkWorld,
    -- * Lenses
    worldMembers,
    worldTags,
    -- * Query
    ) where

-- import Control.Lens
import Futurice.Prelude
import Prelude ()

import Data.Vector.Lens (toVectorOf)

import Jasenrekisteri.Person
import Jasenrekisteri.Tag

data World = World
    { _worldMembers :: !(Vector Person)
    , _worldTags    :: !TagHierarchy
    }

makeLenses ''World

mkWorld
    :: (Foldable f, Foldable f')
    => f Person
    -> f' Tag
    -> World
mkWorld persons tags = World
    { _worldMembers = toVectorOf folded persons
    , _worldTags    = tagHierarchyOf folded tags
    }
