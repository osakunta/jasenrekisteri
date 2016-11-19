{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs , RankNTypes           #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE PolyKinds        #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators, OverloadedStrings    #-}
module SatO.Jasenrekisteri.PersonEdit (
    PersonEdit (..),
    toEndo,
    personEdits,
    personEdits',
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

import SatO.Jasenrekisteri.Person

data PersonEdit = PersonEdit
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

makeLenses ''PersonEdit
deriveGeneric ''PersonEdit

instance ToJSON PersonEdit where
    toJSON     = sopToJSON
    toEncoding = sopToEncoding
instance FromJSON PersonEdit where
    parseJSON = sopParseJSON
instance Arbitrary PersonEdit where
    arbitrary = sopArbitrary

instance Semigroup PersonEdit where
    a <> b = to' $ sappend (from' a) (from' b)
      where
        to' = SOP.to . SOP.SOP . SOP.Z
        from' = SOP.unZ . SOP.unSOP . SOP.from

        sappend :: SOP.All M xs => NP I xs -> NP I xs -> NP I xs
        sappend = SOP.hczipWith (Proxy :: Proxy M) (liftA2 sa)

instance Monoid PersonEdit where
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

toEndo :: PersonEdit -> Person -> Person
toEndo = appEndo . mconcat . SOP.hcollapse . fieldEndos

data PE where
    MkPE :: Text -> Text -> Lens' Person Text -> Lens' PersonEdit (Maybe Text) -> PE

personEdits :: NP (K PE) (UnSingleton (SOP.Code PersonEdit))
personEdits =
    K (MkPE "birthday" "Syntymäpäivä" personBirthday peBirthday) :*
    K (MkPE "birthplace" "Syntymäpaikka" personBirthplace peBirthplace) :*
    K (MkPE "lastName" "Sukunimi" personLastName peLastName) :*
    K (MkPE "firstNames" "Etunimet" personFirstNames peFirstNames) :*
    K (MkPE "matrikkeli" "Matrikkeli" personMatrikkeli peMatrikkeli) :*
    K (MkPE "affiliationDate" "Liittymispäivä" personAffiliationDate peAffiliationDate) :*
    K (MkPE "university" "Yliopisto" personUniversity peUniversity) :*
    K (MkPE "tDK" "Tiedekunta" personTDK peTDK) :*
    K (MkPE "address" "Postiosoite" personAddress peAddress) :*
    K (MkPE "zipcode" "Postinumero" personZipcode peZipcode) :*
    K (MkPE "city" "Kaupinki" personCity peCity) :*
    K (MkPE "country" "Maa" personCountry peCountry) :*
    K (MkPE "email" "Sähköposti" personEmail peEmail) :*
    K (MkPE "phone" "Puhelinnumero" personPhone pePhone) :*
    Nil

personEdits' :: [PE]
personEdits' = SOP.hcollapse personEdits

fieldEndos :: PersonEdit -> NP (K (Endo Person)) (UnSingleton (SOP.Code PersonEdit))
fieldEndos pe = SOP.hmap (K . mk . unK) personEdits
  where
    mk :: PE -> Endo Person
    mk (MkPE _ _ p e) = maybe mempty mk' (pe ^. e)
      where
        mk' x = Endo $ p .~ x

-------------------------------------------------------------------------------
-- Proofs
-------------------------------------------------------------------------------

-- | Check that we have two fields less in PersonEdit than in Person
_proof
    :: Length (UnSingleton (SOP.Code Person))
    :~: 'PS ('PS (Length (UnSingleton (SOP.Code PersonEdit))))
_proof = Refl

-- Move to futurice-prelude
type family Length xs :: Peano where
    Length '[]       = 'PZ
    Length (x ': xs) = 'PS (Length xs)

type family UnSingleton (xs :: [k]) :: k where
    UnSingleton '[x] = x
