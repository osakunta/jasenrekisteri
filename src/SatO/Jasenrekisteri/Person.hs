{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module SatO.Jasenrekisteri.Person (
    -- * Person
    Person(..),
    emptyPerson,
    -- * Person identifier
    PersonId,
    -- * Modifications
    addMagicTags,
    -- * Getters
    personFullName,
    personFullNameHtml,
    -- * Lenses
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
import Control.Lens                  (Getter, contains, to)
import Futurice.Generics
import Futurice.IdMap                (HasKey (..))
import Text.Regex.Applicative.Common (decimal)
import Text.Regex.Applicative.Text   (RE', match, sym)

import qualified Data.Attoparsec.Text as Atto
import qualified Data.Csv             as Csv
import qualified Data.Text            as T

import SatO.Foundation
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

emptyPerson :: PersonId -> Person
emptyPerson memberId = Person memberId
    "" "" "" "" "" "" "" "" mempty "" "" "" "" "" ""

makeLenses ''Person
deriveGeneric ''Person

-- | Strips underlines
personFullName :: Getter Person Text
personFullName = to $ \person -> T.replace "_" "" $
    person ^. personFirstNames <> " " <> person ^. personLastName

personFullNameHtml :: Monad m => Getter Person (HtmlT m ())
personFullNameHtml = to $ \person -> formatName $
    person ^. personFirstNames <> " " <> person ^. personLastName

formatName :: Monad m => Text -> HtmlT m ()
formatName = either (const $ i_ "<???>") id . Atto.parseOnly p
  where
    p = sequenceA_ <$> many (noUnderscore <|> underscored)
    noUnderscore = toHtml <$> some (Atto.satisfy (/= '_'))
    underscored  = span_ [ class_ "underline" ] . toHtml <$> underscored'
    underscored' = Atto.char '_' *> many (Atto.satisfy (/= '_')) <* (() <$ Atto.char '_' <|> Atto.endOfInput)

instance Csv.FromRecord Person
instance Csv.ToRecord Person

instance ToJSON Person where
    toJSON     = sopToJSON
    toEncoding = sopToEncoding
instance FromJSON Person where
    parseJSON = fmap (addTaloTag . addFuksiTag). sopParseJSON

instance HasKey Person where
    type Key Person = UUID
    key = personUuid

addMagicTags :: Person -> Person
addMagicTags = addTaloTag . addFuksiTag

-- TODO: use regexp
addTaloTag :: Person -> Person
addTaloTag p = p & personTags . contains "talo" .~ isTalo
  where
    isTalo = "lapinrinne 1" `T.isInfixOf` T.toLower (p ^. personAddress)

-- | TODO: drop fuksi tags
addFuksiTag :: Person -> Person
addFuksiTag p = case match affYear (p ^. personAffiliationDate) of
    Nothing   -> p
    Just year -> p & personTags . contains (fromString $ "fuksi" ++ show year) .~ True
  where
    affYear :: RE' Int
    affYear = decimalInt *> sym '.' *> decimalInt *> sym '.' *> decimalInt

    decimalInt :: RE' Int
    decimalInt = decimal
