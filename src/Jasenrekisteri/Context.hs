{-# LANGUAGE TemplateHaskell #-}
module Jasenrekisteri.Context (
    -- * Creation
    JasenContext(..),
    ctxMembers,
    ctxTags,
    mkContext,
    -- * Query
    ) where

-- import Control.Lens
import Futurice.Prelude
import Prelude ()

import Data.Vector.Lens (toVectorOf)

import Jasenrekisteri.Person
import Jasenrekisteri.Tag

data JasenContext = JasenContext
    { _ctxMembers :: !(Vector Person)
    , _ctxTags    :: !TagHierarchy
    }

makeLenses ''JasenContext

mkContext
    :: (Foldable f, Foldable f')
    => f Person
    -> f' Tag
    -> JasenContext
mkContext persons tags = JasenContext
    { _ctxMembers = toVectorOf folded persons
    , _ctxTags    = tagHierarchyOf folded tags
    }
