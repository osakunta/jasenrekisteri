{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Jasenrekisteri.Person (
	Person(..),
    personHasTag,
    personTags,
    ) where

import Prelude        ()
import Prelude.Compat

import Control.Lens
import Data.Aeson.TH
import Data.Text     (Text)
import GHC.Generics  (Generic)

import qualified Data.Csv as Csv
import qualified Data.Set as Set

import Jasenrekisteri.AesonUtils
import Jasenrekisteri.Tag

data Person = Person
    { personBirthday        :: !Text
    , personBirthplace      :: !Text
    , personLastName        :: !Text
    , personFirstNames      :: !Text
    , personUnusedField     :: !Text
    , personMatrikkeli      :: !Text
    , personAffiliationDate :: !Text
    , personUniversity      :: !Text
    , personTDK             :: !Text
    , _personTags           :: !Tags
    , personAddress         :: !Text
    , personZipcode         :: !Text
    , personCity            :: !Text
    , personCountry         :: !Text
    , personEmail           :: !Text
    , personPhone           :: !Text
    }
    deriving (Eq, Ord, Show, Read, Generic)

makeLenses ''Person

instance Csv.FromRecord Person
instance Csv.ToRecord Person

$(deriveJSON (recordOptions 6) ''Person)

personHasTag :: Tag -> Person -> Bool
personHasTag tag p = Set.member tag (getTags $ p ^. personTags)
