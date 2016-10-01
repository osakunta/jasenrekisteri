{-# LANGUAGE DeriveGeneric, DataKinds, TypeFamilies     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Jasenrekisteri.Person (
    -- * Person
    Person(..),
    -- * Person identifier
    PersonId,
    -- ** Lenses
    personBirthday,
    personBirthplace,
    personLastName,
    personFirstNames,
    personUnusedField,
    personMatrikkeli,
    personAffiliationDate,
    personUniversity,
    personTDK,
    personTags,
    personAddress,
    personZipcode,
    personCity,
    personCountry,
    personEmail,
    personPhone,
    ) where

import Futurice.Generics
import Futurice.Prelude
import Prelude ()
--import Control.Lens

import qualified Data.Csv as Csv

import Jasenrekisteri.Tag

type PersonId = UUID

data Person = Person
    { _personBirthday        :: !Text
    , _personBirthplace      :: !Text
    , _personLastName        :: !Text
    , _personFirstNames      :: !Text
    , _personUnusedField     :: !Text
    , _personMatrikkeli      :: !Text
    , _personAffiliationDate :: !Text
    , _personUniversity      :: !Text
    , _personTDK             :: !Text
    , _personTags            :: !TagNames
      -- ^ person's direct tags
    , _personAddress         :: !Text
    , _personZipcode         :: !Text
    , _personCity            :: !Text
    , _personCountry         :: !Text
    , _personEmail           :: !Text
    , _personPhone           :: !Text
    }
    deriving (Eq, Ord, Show, Read, Generic)

makeLenses ''Person
deriveGeneric ''Person

instance Csv.FromRecord Person
instance Csv.ToRecord Person

instance ToJSON Person where toJSON = sopToJSON
instance FromJSON Person where parseJSON = sopParseJSON
