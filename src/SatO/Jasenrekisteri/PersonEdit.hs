{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE PolyKinds        #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}
module SatO.Jasenrekisteri.PersonEdit (
    PersonEdit (..),
    toEndo,
    ) where

import Prelude ()
import Futurice.Prelude
import Control.Lens (ASetter', Getting)
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

fieldEndos :: '[xs] ~ SOP.Code PersonEdit => PersonEdit -> NP (K (Endo Person)) xs
fieldEndos pe =
    mk personBirthday peBirthday :*
    mk personBirthplace peBirthplace :*
    mk personLastName peLastName :*
    mk personFirstNames peFirstNames :*
    mk personMatrikkeli peMatrikkeli :*
    mk personAffiliationDate peAffiliationDate :*
    mk personUniversity peUniversity :*
    mk personTDK peTDK :*
    mk personAddress peAddress :*
    mk personZipcode peZipcode :*
    mk personCity peCity :*
    mk personCountry peCountry :*
    mk personEmail peEmail :*
    mk personPhone pePhone :*
    Nil
  where
    mk :: ASetter' Person a -> Getting (Maybe a) PersonEdit (Maybe a) -> K (Endo Person) (Maybe a)
    mk p e = K $ maybe mempty mk' (pe ^. e)
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
