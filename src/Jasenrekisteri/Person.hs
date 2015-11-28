{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Jasenrekisteri.Person (
	Person(..)
    ) where

import Prelude        ()
import Prelude.Compat

import Data.Aeson.TH
import Data.Text     (Text)
import GHC.Generics  (Generic)

import qualified Data.Csv as Csv

import Jasenrekisteri.AesonUtils
import Jasenrekisteri.Tag

data Person = Person
    { personLastName        :: !Text
    , personOrigName        :: !Text
    , personFirstNames      :: !Text
    , personBirthday        :: !Text
    , personBirthplace      :: !Text
    , personAffiliationDate :: !Text
    , personMatrikkeli      :: !Text
    , personBewaNumero      :: !Text
    , personAddress         :: !Text
    , personZipcode         :: !Text
    , personCity            :: !Text
    , personCountry         :: !Text
    , personEmail           :: !Text
    , personPhone           :: !Text
    , personUniversity      :: !Text
    , personTDK             :: !Text
    , personTags            :: !Tags
    }
    deriving (Eq, Ord, Show, Read, Generic)

instance Csv.FromRecord Person
instance Csv.ToRecord Person

$(deriveJSON (recordOptions 6) ''Person)
