{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module SatO.Jasenrekisteri.MemberEdit (
    MemberEdit (..),
    toEndo,
    memberEdits,
    memberEdits',
    PE (..),
    -- * internal
    UnSingleton,
    ) where

import Prelude ()
import Futurice.Prelude
import Control.Applicative (liftA2)
import Data.Monoid         (Endo (..))
import Data.Type.Equality
import Futurice.Generics
import Futurice.Peano

import qualified Generics.SOP as SOP

import SatO.Jasenrekisteri.Member

data MemberEdit = MemberEdit
    { _peBirthday        :: !(Maybe Text)
    , _peBirthplace      :: !(Maybe Text)
    , _peLastName        :: !(Maybe Text)
    , _peFirstNames      :: !(Maybe Text)
    , _peMatrikkeli      :: !(Maybe Text)
    , _peAffiliationDate :: !(Maybe Text)
    , _peUniversity      :: !(Maybe Text)
    , _peTDK             :: !(Maybe Text)
    , _peAddress         :: !(Maybe Text)
    , _peZipcode         :: !(Maybe Text)
    , _peCity            :: !(Maybe Text)
    , _peCountry         :: !(Maybe Text)
    , _peEmail           :: !(Maybe Text)
    , _pePhone           :: !(Maybe Text)
    }
  deriving (Eq, Ord, Show, Read, Generic)

makeLenses ''MemberEdit
deriveGeneric ''MemberEdit

instance ToJSON MemberEdit where
    toJSON     = sopToJSON
    toEncoding = sopToEncoding
instance FromJSON MemberEdit where
    parseJSON = sopParseJSON
instance Arbitrary MemberEdit where
    arbitrary = sopArbitrary

instance Semigroup MemberEdit where
    a <> b = to' $ sappend (from' a) (from' b)
      where
        to' = SOP.to . SOP.SOP . SOP.Z
        from' = SOP.unZ . SOP.unSOP . SOP.from

        sappend :: SOP.All M xs => NP I xs -> NP I xs -> NP I xs
        sappend = SOP.hczipWith (Proxy :: Proxy M) (liftA2 sa)

instance Monoid MemberEdit where
    mempty  = SOP.to . SOP.SOP . SOP.Z $ SOP.hcpure (Proxy :: Proxy M) (I se)
    mappend = (<>)

class M a where
    se :: a
    sa :: a -> a -> a

-- | Acts like last
instance M (Maybe a) where
    se = Nothing

    sa x Nothing = x
    sa _       y = y

toEndo :: MemberEdit -> Member -> Member
toEndo = appEndo . mconcat . SOP.hcollapse . fieldEndos

data PE where
    MkPE :: Text -> Text -> Lens' Member Text -> Lens' MemberEdit (Maybe Text) -> PE

memberEdits :: NP (K PE) (UnSingleton (SOP.Code MemberEdit))
memberEdits =
    K (MkPE "birthday" "Syntymäpäivä" memberBirthday peBirthday) :*
    K (MkPE "birthplace" "Syntymäpaikka" memberBirthplace peBirthplace) :*
    K (MkPE "lastName" "Sukunimi" memberLastName peLastName) :*
    K (MkPE "firstNames" "Etunimet" memberFirstNames peFirstNames) :*
    K (MkPE "matrikkeli" "Matrikkeli" memberMatrikkeli peMatrikkeli) :*
    K (MkPE "affiliationDate" "Liittymispäivä" memberAffiliationDate peAffiliationDate) :*
    K (MkPE "university" "Yliopisto" memberUniversity peUniversity) :*
    K (MkPE "tDK" "Tiedekunta" memberTDK peTDK) :*
    K (MkPE "address" "Postiosoite" memberAddress peAddress) :*
    K (MkPE "zipcode" "Postinumero" memberZipcode peZipcode) :*
    K (MkPE "city" "Kaupinki" memberCity peCity) :*
    K (MkPE "country" "Maa" memberCountry peCountry) :*
    K (MkPE "email" "Sähköposti" memberEmail peEmail) :*
    K (MkPE "phone" "Puhelinnumero" memberPhone pePhone) :*
    Nil

memberEdits' :: [PE]
memberEdits' = SOP.hcollapse memberEdits

fieldEndos :: MemberEdit -> NP (K (Endo Member)) (UnSingleton (SOP.Code MemberEdit))
fieldEndos pe = SOP.hmap (K . mk . unK) memberEdits
  where
    mk :: PE -> Endo Member
    mk (MkPE _ _ p e) = maybe mempty mk' (pe ^. e)
      where
        mk' x = Endo $ p .~ x

-------------------------------------------------------------------------------
-- Proofs
-------------------------------------------------------------------------------

-- | Check that we have two fields less in MemberEdit than in Member
_proof
    :: Length (UnSingleton (SOP.Code Member))
    :~: 'PS ('PS (Length (UnSingleton (SOP.Code MemberEdit))))
_proof = Refl

-- Move to futurice-prelude
type family Length xs :: Peano where
    Length '[]       = 'PZ
    Length (x ': xs) = 'PS (Length xs)

type family UnSingleton (xs :: [k]) :: k where
    UnSingleton '[x] = x
