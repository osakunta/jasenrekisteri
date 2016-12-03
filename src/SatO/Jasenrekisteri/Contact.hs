{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module SatO.Jasenrekisteri.Contact (
    Contact (..),
    contactFromMember,
    SearchResult (..),
    ) where

import Prelude ()
import Futurice.Prelude
import Control.Lens                     ((?=))
import Codec.Xlsx
import Control.Monad.Trans.State.Strict (execState)
import Futurice.Generics
import Servant.Xlsx                     (ToXlsx (..))

import SatO.Jasenrekisteri.Member
import SatO.Jasenrekisteri.SearchQuery

data Contact = Contact
    { contactName    :: !Text
    , contactAddress :: !Text
    , contactZipcode :: !Text
    , contactCity    :: !Text
    }

contactFromMember :: Member -> Contact
contactFromMember p = Contact
    { contactName    = p ^. memberFullName
    , contactAddress = p ^. memberAddress
    , contactZipcode = p ^. memberZipcode
    , contactCity    = p ^. memberCity
    }

deriveGeneric ''Contact

instance ToNamedRecord  Contact where toNamedRecord  = sopToNamedRecord
instance DefaultOrdered Contact where headerOrder    = sopHeaderOrder

-------------------------------------------------------------------------------
-- SearchResult
-------------------------------------------------------------------------------

data SearchResult = SearchResult
    { srSearchQuery :: !SearchQuery
    , srContacts    :: ![Contact]
    }

instance ToXlsx SearchResult where
    toXlsx (SearchResult query contacts) = def
        & atSheet (prettySearchQuery query) ?~ sheet
      where
        sheet = flip execState def $ ifor_ contacts $ \i contact -> do
            cellValueAt (i + 1, 1) ?= CellText (contactName contact)
            cellValueAt (i + 1, 2) ?= CellText (contactAddress contact)
            cellValueAt (i + 1, 3) ?= CellText (contactZipcode contact)
            cellValueAt (i + 1, 4) ?= CellText (contactCity contact)
