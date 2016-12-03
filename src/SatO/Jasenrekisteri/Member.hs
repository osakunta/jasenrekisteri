{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module SatO.Jasenrekisteri.Member (
    -- * Member
    Member(..),
    emptyMember,
    -- * Member identifier
    MemberId,
    -- * Modifications
    addMagicTags,
    modifyTaloAddress,
    -- * Getters
    memberFullName,
    memberFullNameHtml,
    memberSortKey,
    -- * Lenses
    memberUuid,
    memberBirthday,
    memberBirthplace,
    memberLastName,
    memberFirstNames,
    memberMatrikkeli,
    memberAffiliationDate,
    memberUniversity,
    memberTDK,
    memberTags,
    memberAddress,
    memberZipcode,
    memberCity,
    memberCountry,
    memberEmail,
    memberPhone,
    ) where

import Prelude ()
import Futurice.Prelude
import Control.Lens                  (Getter, contains, re, to, (%~))
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

type MemberId = UUID

data Member = Member
    { _memberUuid            :: !MemberId
    , _memberBirthday        :: !Text
    , _memberBirthplace      :: !Text
    , _memberLastName        :: !Text
    , _memberFirstNames      :: !Text
    , _memberMatrikkeli      :: !Text
    , _memberAffiliationDate :: !Text
    , _memberUniversity      :: !Text
    , _memberTDK             :: !Text
    , _memberTags            :: !TagNames
      -- ^ member's direct tags
    , _memberAddress         :: !Text
    , _memberZipcode         :: !Text
    , _memberCity            :: !Text
    , _memberCountry         :: !Text
    , _memberEmail           :: !Text
    , _memberPhone           :: !Text
    }
    deriving (Eq, Ord, Show, Read, Generic)

emptyMember :: MemberId -> Member
emptyMember memberId = Member memberId
    "" "" "" "" "" "" "" "" mempty "" "" "" "" "" ""

makeLenses ''Member
deriveGeneric ''Member

-- | Strips underlines
memberFullName :: Getter Member Text
memberFullName = to $ \member -> T.replace "_" "" $
    member ^. memberFirstNames <> " " <> member ^. memberLastName

memberSortKey :: Getter Member [Text]
memberSortKey = to $ \member ->
    member ^. memberLastName
    : T.splitOn " " (T.replace "_" "" $ member ^. memberFirstNames)

memberFullNameHtml :: Monad m => Getter Member (HtmlT m ())
memberFullNameHtml = to $ \member -> formatName $
    member ^. memberFirstNames <> " " <> member ^. memberLastName

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

instance Csv.FromRecord Member
instance Csv.ToRecord Member

instance ToJSON Member where
    toJSON     = sopToJSON
    toEncoding = sopToEncoding
instance FromJSON Member where
    parseJSON = fmap addMagicTags . sopParseJSON

instance HasKey Member where
    type Key Member = UUID
    key = memberUuid

addMagicTags :: Member -> Member
addMagicTags = addTaloTag . addFuksiTag . addSchoolTag

-- TODO: use regexp
addTaloTag :: Member -> Member
addTaloTag p = p & memberTags . contains "talo" .~ isTalo
  where
    isTalo = "lapinrinne 1" `T.isInfixOf` T.toLower (p ^. memberAddress)

-- | TODO: drop fuksi tags
addFuksiTag :: Member -> Member
addFuksiTag p = case match affYear (p ^. memberAffiliationDate) of
    Nothing   -> p
    Just year -> p & memberTags . contains (fromString $ "fuksi" ++ show year) .~ True
  where
    affYear :: RE' Int
    affYear = decimalInt *> sym '.' *> decimalInt *> sym '.' *> decimalInt

    decimalInt :: RE' Int
    decimalInt = decimal

addSchoolTag :: Member -> Member
addSchoolTag p = p & memberTags %~ addSchools
  where
    schools = fmap T.strip . T.splitOn "," $ p ^. memberUniversity
    addSchools ts = ts <> tagNamesOf (folded . re _TagName) schools

-- todo: use regexp
modifyTaloAddress :: Text -> Text
modifyTaloAddress = T.replace "Lapinrinne 1 " ""
