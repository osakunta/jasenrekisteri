{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
-- | The tag colours and hierarchy
module SatO.Jasenrekisteri.Hierarchy (tags) where

import Prelude ()
import Futurice.Prelude
import Control.Lens
import Data.Set.Lens           (setOf)
import SatO.Jasenrekisteri.Tag

import           Futurice.IdMap (IdMap)
import qualified Futurice.IdMap as IdMap

tags :: [Tag]
tags =
    merkit
    <> [taloTag, senioriTag]
    <> kuraattoriTags
    <> fuksiTags
    <> toList virkailijat
    <> jasenTags

-------------------------------------------------------------------------------
-- Virkailijat
-------------------------------------------------------------------------------

virkailijat :: IdMap Tag
virkailijat = m'
  where
    m = foldMap virkailijatPerYear years <> parentVirat
    m' = foldr insertChildren m
        [ (year, virka)
        | year  <- years
        , virka <- hallitusVirat
        ]

    insertChildren (year, virka) vm =
        vm & ix parent . tagChildren . contains child .~ True
      where
        parent = TagName virka
        child  = TagName $ virka <> textShow year

    years :: [Int]
    years = [ 2006..2017 ]

    parentVirat :: IdMap Tag
    parentVirat = IdMap.fromFoldable $
        [ Tag (TagName virka) hallitusColour mempty
        | virka <- hallitusVirat
        ]

    virkailijatPerYear :: Int -> IdMap Tag
    virkailijatPerYear year = IdMap.fromFoldable $
        [ Tag virkailijatName virkailijaColour $ TagNames $
              setOf folded [ hallitusName ]
        , Tag hallitusName hallitusColour $ TagNames $
              setOf (folded . tagName) hallitus
        ]
        ++ hallitus
      where
        virkailijatName = _TagName # ("virkailijat" <> tyear)
        hallitusName    = _TagName # ("hallitus" <> tyear)
        tyear           = textShow year

        hallitus :: [Tag]
        hallitus = flip map hallitusVirat $ \v ->
            Tag (_TagName # (v <> tyear)) 0 mempty

hallitusVirat :: [Text]
hallitusVirat =
    [ "hpj"
    , "pääsihteeri"
    , "isäntä"
    , "emäntä"
    , "tiedotussihteeri"
    -- , "opastussihteeri"
    ]

-------------------------------------------------------------------------------
-- Kuraattori
-------------------------------------------------------------------------------

kuraattoriTags :: [Tag]
kuraattoriTags = t : ts
  where
    t = Tag "kuraattori" merkkiColour (tagNamesOf (folded . tagName) ts)
    ts = kuraattoriTag <$> [ 2001, 2003 .. 2019 ]

    kuraattoriTag :: Int -> Tag
    kuraattoriTag year = Tag (_TagName # ("kuraattori" <> textShow year <> "-" <> textShow (year + 1))) merkkiColour mempty


-------------------------------------------------------------------------------
-- Fuksi
-------------------------------------------------------------------------------

fuksiTags :: [Tag]
fuksiTags = fuksiTag <$> [ 2000 .. 2017 ]
  where
    fuksiTag :: Int -> Tag
    fuksiTag year = Tag (_TagName # ("fuksi" <> textShow year)) fuksiColour mempty

-------------------------------------------------------------------------------
-- Jäsenmaksun maksaneet
-------------------------------------------------------------------------------

jasenTags :: [Tag]
jasenTags = jasenTag <$> [ 1996..2017 ]
  where
    jasenTag :: Int -> Tag
    jasenTag year = Tag (_TagName # name) jasenColour mempty
      where
        name = a <> "-" <> b
        a = textShow year
        b = textShow (year + 1)

-------------------------------------------------------------------------------
-- Seniori
-------------------------------------------------------------------------------

senioriTag :: Tag
senioriTag = Tag "seniori" senioriColour mempty

-------------------------------------------------------------------------------
-- Talo
-------------------------------------------------------------------------------

taloTag :: Tag
taloTag = Tag "talo" taloColour mempty

-------------------------------------------------------------------------------
-- Merkit
-------------------------------------------------------------------------------

merkit :: [Tag]
merkit =
    [ Tag "ansiomerkki" merkkiColour mempty
    , Tag "harrastusmerkki" merkkiColour mempty
    ]

-------------------------------------------------------------------------------
-- Colours
-------------------------------------------------------------------------------

senioriColour :: TagColour
senioriColour = 1

hallitusColour :: TagColour
hallitusColour = 7

virkailijaColour :: TagColour
virkailijaColour = 6

fuksiColour :: TagColour
fuksiColour = 3

merkkiColour :: TagColour
merkkiColour = 2

taloColour :: TagColour
taloColour = 9

jasenColour :: TagColour
jasenColour = 4
