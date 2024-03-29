{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
-- | The tag colours and hierarchy
module SatO.Jasenrekisteri.Hierarchy (tags) where

import Prelude ()
import Futurice.Prelude
import Control.Lens              hiding ((...))
import Data.Maybe                (listToMaybe, mapMaybe)
import Futurice.IdMap            (IdMap)
import Numeric.Interval.NonEmpty (Interval, inf, sup, (...))
import SatO.Jasenrekisteri.Tag

import qualified Futurice.IdMap            as IdMap
import qualified Numeric.Interval.NonEmpty as Interval

tags :: [Tag]
tags =
    merkit
    <> [taloTag, osakehuoneistoTag, senioriTag, kuollutTag, eiosoitettaTag]
    <> schools
    <> kuraattoriTags
    <> fuksiTags
    <> toList virkailijat
    <> jasenTags

-------------------------------------------------------------------------------
-- Virkailijat
-------------------------------------------------------------------------------

virkailijat :: IdMap Tag
virkailijat = IdMap.fromFoldable $ virat ++ virat2 ++ hallitus
  where
    virat :: [Tag]
    virat = flip concatMap kaikkiVirat $ \(name, years, additionalChildren) ->
        let years'        = intervalSpan years
            tagName'      = TagName name
            childTags     = map makeTag years'

            makeTag y     = Tag (TagName $ name <> textShow y) colour mempty
              where
                colour = if any p hallitusVirat
                    then hallitusColour
                    else virkailijaColour
                p (n, ys) = n == name && Interval.member y ys

            -- Colour of the parent tag, e.g. "hpj"
            parentColour  =
                if maybe False (intervalEq years) $ listToMaybe $ mapMaybe pickk hallitusVirat
                    then hallitusColour
                    else virkailijaColour
              where
                -- pick years for this duty
                pickk (n, ys)
                    | n == name = Just ys
                    | otherwise = Nothing

        in Tag tagName' parentColour (tagNamesOf (folded . tagName) childTags <> tagNamesOf folded additionalChildren) : childTags

    virat2 :: [Tag]
    virat2 = flip map [virkailijatFrom .. virkailijatTo] $ \year ->
        Tag (TagName $ "virkailijat" <> textShow year) virkailijaColour $ tagNamesOf folded $
            (:) (TagName $ "hallitus" <> textShow year) $ flip mapMaybe kaikkiVirat $ \(name, years, _) ->
                if Interval.member year years
                    then Just $ TagName $ name <> textShow year
                    else Nothing

    hallitus :: [Tag]
    hallitus = flip map [virkailijatFrom .. virkailijatTo] $ \year ->
        Tag (TagName $ "hallitus" <> textShow year) hallitusColour $ tagNamesOf folded $
            flip mapMaybe hallitusVirat $ \(name, years) ->
                if Interval.member year years
                    then Just $ TagName $ name <> textShow year
                    else Nothing

hallitusVirat :: [(Text, Interval Int)]
hallitusVirat =
    [ mk "hpj"              (past ... future)
    , mk "pääsihteeri"      (past ... future)
    , mk "isäntä"           (past ... future)
    , mk "emäntä"           (past ... future)
    , mk "tiedotussihteeri" (past ... future)
    , mk "opastussihteeri"  (past ... 2011)
    ]
  where
    mk = (,)
    future = virkailijatTo
    past   = virkailijatFrom

kaikkiVirat :: [(Text, Interval Int, [TagName])]
kaikkiVirat =
    [ mk "hpj"                          (past ... future) []
    , mk "pääsihteeri"                  (past ... future) []
    , mk "isäntä"                       (past ... future) []
    , mk "emäntä"                       (past ... future) []
    , mk "tiedotussihteeri"             (past ... future) []
    , mk "päätoimittaja"                (past ... future) []
    , mk "taloudenhoitaja"              (past ... future) []
    , mk "urheiluohjaaja"               (past ... future) ["naistenurheiluohjaaja", "mistenurheiluohjaaja"]
    , mk "kulttuurisihteeri"            (past ... future) []
    , mk "kappalainen"                  (past ... future) []
    , mk "yhteiskuntasihteeri"          (past ... future) []
    , mk "kirjastonhoitaja"             (past ... future) []
    , mk "opastussihteeri"              (past ... future) []
    , mk "jäsensihteeri"                (2012 ... future) ["apulaissihteeri"]
    , mk "mainossihteeri"               (past ... future) []
    , mk "historioitsija"               (past ... future) []
    , mk "galleristi"                   (2012 ... future) ["arkistonhoitaja"]
    , mk "valokuvaaja"                  (past ... future) []
    , mk "laulunjohtaja"                (past ... future) []
    , mk "tietotekniikkavastaava"       (2012 ... future) ["otso"]
    , mk "verkkovastaava"               (2012 ... future) ["urho"]
    , mk "musiikkihuoneenhoitaja"       (past ... future) []
    , mk "haronmäenisäntä"              (past ... future) []
    , mk "valvontatilintarkastaja"      (past ... future) []
    , mk "varavalvontatilintarkastaja"  (past ... future) []
    , mk "seniorisihteeri"              (2017 ... future) []
    , mk "juhlamestari"                 (past ... future) []
    , mk "apulaissihteeri"              (past ... 2011) []
    , mk "ulkoasiainsihteeri"           (past ... 2011) []
    , mk "naistenurheiluohjaaja"        (past ... 2011) []
    , mk "miestenurheiluohjaaja"        (past ... 2011) []
    , mk "arkistonhoitaja"              (past ... 2011) []
    , mk "julkaisuvarastonhoitaja"      (past ... 2011) []
    , mk "lehtihuoneenhoitaja"          (past ... 2011) []
    , mk "urho"                         (past ... 2011) []
    , mk "otso"                         (past ... 2011) []
    , mk "kamreeri"                     (2018 ... future) []
    ]
  where
    mk = (,,)
    future = virkailijatTo
    past   = virkailijatFrom

virkailijatFrom :: Int
virkailijatFrom = 2008

virkailijatTo :: Int
virkailijatTo = 2022

-------------------------------------------------------------------------------
-- Kuraattori
-------------------------------------------------------------------------------

kuraattoriTags :: [Tag]
kuraattoriTags = t : ts
  where
    t = Tag "kuraattori" merkkiColour (tagNamesOf (folded . tagName) ts)
    ts = kuraattoriTag <$> [ 1997, 1999 .. 2023 ]

    kuraattoriTag :: Int -> Tag
    kuraattoriTag year = Tag (_TagName # ("kuraattori" <> textShow year <> "-" <> textShow (year + 1))) merkkiColour mempty


-------------------------------------------------------------------------------
-- Fuksi
-------------------------------------------------------------------------------

fuksiTags :: [Tag]
fuksiTags = fuksiTag <$> [ 2000 .. 2022 ]
  where
    fuksiTag :: Int -> Tag
    fuksiTag year = Tag (_TagName # ("fuksi" <> textShow year)) fuksiColour mempty

-------------------------------------------------------------------------------
-- Jäsenmaksun maksaneet
-------------------------------------------------------------------------------

jasenTags :: [Tag]
jasenTags = jasenTag <$> [ 1996..2022 ]
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

osakehuoneistoTag :: Tag
osakehuoneistoTag = Tag "osakehuoneisto" taloColour mempty

-------------------------------------------------------------------------------
-- Merkit
-------------------------------------------------------------------------------

merkit :: [Tag]
merkit =
    [ Tag "ansiomerkki" merkkiColour mempty
    , Tag "harrastusmerkki" merkkiColour mempty
    ]

-------------------------------------------------------------------------------
-- kuollut
-------------------------------------------------------------------------------

kuollutTag :: Tag
kuollutTag = Tag "kuollut" kuollutColour mempty

eiosoitettaTag :: Tag
eiosoitettaTag = Tag "eiosoitetta" kuollutColour mempty

-------------------------------------------------------------------------------
-- Schools
 -------------------------------------------------------------------------------

schools :: [Tag]
schools = map (\n -> Tag n schoolColour mempty)
    [ "Aalto"
    , "HKKK"
    , "HY"
    , "Haaga-Helia"
    , "Hanken"
    , "JYI"
    , "Laurea"
    , "MPKK"
    , "Metropolia"
    , "SAMK"
    , "Siba"
    , "TAIK"
    , "TEAK"
    , "TKK"
    , "TTY"
    , "Taideyliopisto"
    , "UTU"
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

schoolColour :: TagColour
schoolColour = 10

kuollutColour :: TagColour
kuollutColour = 11

-------------------------------------------------------------------------------
-- Intervals
-------------------------------------------------------------------------------

intervalSpan :: Enum a => Interval a -> [a]
intervalSpan a = [inf a .. sup a]

-- https://github.com/ekmett/intervals/pull/48
intervalEq :: Eq a => Interval a -> Interval a -> Bool
intervalEq a b = sup a == sup b && inf a == inf b
