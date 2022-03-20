{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module SatO.Jasenrekisteri.DumpMember (
    DumpMember (..),
    dumpMember,
    DumpMembers (..),
) where

import Codec.Xlsx
import Control.Lens                     ((?=))
import Control.Monad.Trans.State.Strict (execState)
import Futurice.Generics
import Futurice.Generics.SOP            (sopHeaderOrder, sopToNamedRecord)
import Futurice.Prelude
import Prelude ()
import Servant.Xlsx                     (ToXlsx (..))

import qualified Data.UUID as UUID

import SatO.Jasenrekisteri.Member
import SatO.Jasenrekisteri.Tag

data DumpMember = DumpMember
    { _dmUuid            :: !MemberId
    , _dmFirstNames      :: !Text
    , _dmLastName        :: !Text
    , _dmBirthday        :: !Text
    , _dmBirthplace      :: !Text
    , _dmMatrikkeli      :: !Text
    , _dmAffiliationDate :: !Text
    , _dmUniversity      :: !Text
    , _dmTDK             :: !Text
    , _dmAddress         :: !Text
    , _dmZipcode         :: !Text
    , _dmCity            :: !Text
    , _dmCountry         :: !Text
    , _dmEmail           :: !Text
    , _dmPhone           :: !Text
    , _dmTags            :: !TagNames
    , _dmAllTags         :: !TagNames
    -- ^ member's direct tags
    }
  deriving (Eq, Ord, Show, Read, Generic)

deriveGeneric ''DumpMember

instance ToNamedRecord  DumpMember where toNamedRecord  = sopToNamedRecord
instance DefaultOrdered DumpMember where headerOrder    = sopHeaderOrder

dumpMember :: Member -> TagNames -> DumpMember
dumpMember Member {..} _dmAllTags = DumpMember {..} where
    _dmUuid            = _memberUuid
    _dmFirstNames      = _memberFirstNames
    _dmLastName        = _memberLastName
    _dmBirthday        = _memberBirthday
    _dmBirthplace      = _memberBirthplace
    _dmMatrikkeli      = _memberMatrikkeli
    _dmAffiliationDate = _memberAffiliationDate
    _dmUniversity      = _memberUniversity
    _dmTDK             = _memberTDK
    _dmAddress         = _memberAddress
    _dmZipcode         = _memberZipcode
    _dmCity            = _memberCity
    _dmCountry         = _memberCountry
    _dmEmail           = _memberEmail
    _dmPhone           = _memberPhone
    _dmTags            = _memberTags

-------------------------------------------------------------------------------
-- DumpMembers
-------------------------------------------------------------------------------

newtype DumpMembers = DumpMembers [DumpMember]

instance ToXlsx DumpMembers where
    toXlsx (DumpMembers members) = def
        & atSheet "Jasenet" ?~ sheet
      where
        sheet = flip execState def $ ifor_ members $ \i DumpMember {..}-> do
            cellValueAt (i + 1, 1)  ?= CellText (UUID.toText _dmUuid)
            cellValueAt (i + 1, 2)  ?= CellText (_dmFirstNames)
            cellValueAt (i + 1, 3)  ?= CellText (_dmLastName)
            cellValueAt (i + 1, 4)  ?= CellText (_dmBirthday)
            cellValueAt (i + 1, 5)  ?= CellText (_dmBirthplace)
            cellValueAt (i + 1, 6)  ?= CellText (_dmMatrikkeli)
            cellValueAt (i + 1, 7)  ?= CellText (_dmAffiliationDate)
            cellValueAt (i + 1, 8)  ?= CellText (_dmUniversity)
            cellValueAt (i + 1, 9)  ?= CellText (_dmTDK)
            cellValueAt (i + 1, 10) ?= CellText (_dmAddress)
            cellValueAt (i + 1, 11) ?= CellText (_dmZipcode)
            cellValueAt (i + 1, 12) ?= CellText (_dmCity)
            cellValueAt (i + 1, 13) ?= CellText (_dmCountry)
            cellValueAt (i + 1, 14) ?= CellText (_dmEmail)
            cellValueAt (i + 1, 15) ?= CellText (_dmPhone)
            cellValueAt (i + 1, 16) ?= CellText (tagNamesToText _dmTags)
            cellValueAt (i + 1, 17) ?= CellText (tagNamesToText _dmAllTags)
