{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module SatO.Jasenrekisteri.Person (
    -- * Person
    Person(..),
    -- * Person identifier
    PersonId,
    -- ** Lenses
    personUuid,
    personBirthday,
    personBirthplace,
    personLastName,
    personFirstNames,
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

import Prelude ()
import Futurice.Prelude
import Control.Lens
import Futurice.Generics
import Futurice.IdMap                (HasKey (..))
import Text.Regex.Applicative.Common (decimal)
import Text.Regex.Applicative.Text   (RE', match, sym)

import qualified Data.Csv  as Csv
import qualified Data.Text as T

import SatO.Jasenrekisteri.Tag

type PersonId = UUID

data Person = Person
    { _personUuid            :: !PersonId
    , _personBirthday        :: !Text
    , _personBirthplace      :: !Text
    , _personLastName        :: !Text
    , _personFirstNames      :: !Text
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
instance FromJSON Person where
    parseJSON = fmap (addTaloTag . addFuksiTag). sopParseJSON

instance HasKey Person where
    type Key Person = UUID
    key = personUuid

-- | TODO: use regexp
addTaloTag :: Person -> Person
addTaloTag p
    | "lapinrinne 1" `T.isInfixOf` T.toLower (p ^. personAddress)
        = p & personTags . contains "talo" .~ True
    | otherwise
        = p

addFuksiTag :: Person -> Person
addFuksiTag p = case match affYear (p ^. personAffiliationDate) of
    Nothing   -> p
    Just year -> p & personTags . contains (fromString $ "fuksi" ++ show year) .~ True
  where
    affYear :: RE' Int
    affYear = decimalInt *> sym '.' *> decimalInt *> sym '.' *> decimalInt

    decimalInt :: RE' Int
    decimalInt = decimal
