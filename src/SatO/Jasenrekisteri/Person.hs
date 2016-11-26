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
import Control.Lens                  (Getter, contains, to, re, (%~))
import Data.Char                     (isLetter)
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
formatName = either (const $ i_ "<???>") id . Atto.parseOnly (p <* Atto.endOfInput)
  where
    p = sequenceA_ <$> many (underscored <|> asterisk <|> noUnderscore)
    noUnderscore = toHtml <$> some (Atto.satisfy (`notElem` ['_', '*']))
    -- _Foo_
    underscored  = span_ [ class_ "underline" ] . toHtml <$> underscored'
    underscored' = Atto.char '_' *> many (Atto.satisfy (/= '_')) <* (() <$ Atto.char '_' <|> Atto.endOfInput)
    -- *Foo
    asterisk     = span_ [ class_ "underline" ] . toHtml <$> asterisk'
    asterisk'    = Atto.char '*' *> many (Atto.satisfy isLetter)

instance Csv.FromRecord Person
instance Csv.ToRecord Person

instance ToJSON Person where
    toJSON     = sopToJSON
    toEncoding = sopToEncoding
instance FromJSON Person where
    parseJSON = fmap addMagicTags . sopParseJSON

instance HasKey Person where
    type Key Person = UUID
    key = personUuid

addMagicTags :: Person -> Person
addMagicTags = addTaloTag . addFuksiTag . addSchoolTag

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

addSchoolTag :: Person -> Person
addSchoolTag p = p & personTags %~ addSchools
  where
    schools = fmap T.strip . T.splitOn "," $ p ^. personUniversity
    addSchools ts = ts <> tagNamesOf (folded . re _TagName) schools
