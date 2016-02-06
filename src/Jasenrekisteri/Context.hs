module Jasenrekisteri.Context (
    -- * Creation
    JasenContext(..),
    mkContext,
    -- * Query
    ) where

import Prelude        ()
import Prelude.Compat

import Control.Lens
import Data.HashMap.Strict (HashMap)
import Data.Maybe          (fromMaybe)
import Data.Vector         (Vector)

import qualified Data.HashMap.Strict as HM
import qualified Data.Vector         as V
--import qualified Data.Set as Set

import Jasenrekisteri.Person
import Jasenrekisteri.Tag

data JasenContext = JasenContext
    { ctxMembers         :: !(Vector Person)
    , ctxTags            :: !TagHierarchy
    , ctxColours         :: !TagColours
    , ctxDeepTags        :: !TagHierarchy
    , ctxMembersDeepTags :: !(Vector Person)
    , ctxMembersByTag    :: !(HashMap Tag (Vector Person))
    }

mkContext :: Vector Person -> TagHierarchy -> TagColours -> JasenContext
mkContext persons tags colours = JasenContext
    { ctxMembers         = persons
    , ctxTags            = tags
    , ctxColours         = colours
    , ctxDeepTags        = deepTags
    , ctxMembersDeepTags = deepPersons
    , ctxMembersByTag    = membersByTag'
    }
  where
    deepTags         = mkDeepTags tags
    deepPersons      = V.map (mkMemberWithDeepTags deepTags) persons
    membersByTag tag = V.filter (personHasTag tag) persons
    membersByTag' = HM.fromList
                  . map (\tag -> (tag, membersByTag tag))
                  . HM.keys
                  . getChildTags
                   $ tags

tagHierarchyLookup :: TagHierarchy -> Tag -> Tags
tagHierarchyLookup th tag = fromMaybe (Tags mempty) (HM.lookup tag $ getChildTags th)

mkDeepTags :: TagHierarchy -> TagHierarchy
mkDeepTags th = TagHierarchy . HM.map iter . getChildTags $ th
  where iter tags = let nextTags = foldMap (tagHierarchyLookup th) $ getTags tags
                    in if tagsLength tags == tagsLength nextTags
                           then tags
                           else iter nextTags

mkMemberWithDeepTags :: TagHierarchy -> Person -> Person
mkMemberWithDeepTags th p = p & personTags %~ foldMap (tagHierarchyLookup th) . getTags
