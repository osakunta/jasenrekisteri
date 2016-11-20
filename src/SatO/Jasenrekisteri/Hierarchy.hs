{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
-- | The tag colours and hierarchy
module SatO.Jasenrekisteri.Hierarchy (tags) where

import Prelude ()
import Futurice.Prelude
import Control.Lens              hiding ((...))
import Data.Maybe                (mapMaybe)
import Futurice.IdMap            (IdMap)
import Numeric.Interval.NonEmpty (Interval, inf, sup, (...))
import SatO.Jasenrekisteri.Tag

import qualified Futurice.IdMap            as IdMap
import qualified Numeric.Interval.NonEmpty as Interval

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
virkailijat = IdMap.fromFoldable $ virat ++ virat2 ++ hallitus
  where
    virat :: [Tag]
    virat = flip concatMap kaikkiVirat $ \(name, years) ->
        let years'        = intervalSpan years
            tagName'      = TagName name
            childTagNames = map (\y -> TagName $ name <> textShow y) years'
            -- TODO:
            colour        = virkailijaColour
        in Tag tagName' colour (tagNamesOf folded childTagNames)
            : map (\n -> Tag n colour mempty) childTagNames

    virat2 :: [Tag]
    virat2 = flip map [virkailijatFrom .. virkailijatTo] $ \year ->
        Tag (TagName $ "virkailijat" <> textShow year) virkailijaColour $ tagNamesOf folded $
            flip mapMaybe kaikkiVirat $ \(name, years) ->
                if Interval.elem year years
                    then Just $ TagName $ name <> textShow year
                    else Nothing

    hallitus :: [Tag]
    hallitus = flip map [virkailijatFrom .. virkailijatTo] $ \year ->
        Tag (TagName $ "hallitus" <> textShow year) hallitusColour $ tagNamesOf folded $
            flip mapMaybe hallitusVirat $ \(name, years) ->
                if Interval.elem year years
                    then Just $ TagName $ name <> textShow year
                    else Nothing

intervalSpan :: Enum a => Interval a -> [a]
intervalSpan a = [inf a .. sup a]

hallitusVirat :: [(Text, Interval Int)]
hallitusVirat =
    [ mk "hpj"              (past ... 2011)
    , mk "pääsihteeri"      (past ... 2011)
    , mk "isäntä"           (past ... 2011)
    , mk "emäntä"           (past ... 2011)
    , mk "tiedotussihteeri" (past ... future)
    , mk "opastussihteeri"  (past ... 2011)
    ]
  where
    mk = (,)
    future = virkailijatTo
    past   = virkailijatFrom

kaikkiVirat :: [(Text, Interval Int)]
kaikkiVirat =
    [ mk "hpj"                          (past ... future)
    , mk "pääsihteeri"                  (past ... future)
    , mk "isäntä"                       (past ... future)
    , mk "emäntä"                       (past ... future)
    , mk "tiedotussihteeri"             (past ... future)
    , mk "päätoimittaja"                (past ... future)
    , mk "taloudenhoitaja"              (past ... future)
    , mk "urheiluohjaaja"               (past ... future)
    , mk "kulttuurisihteeri"            (past ... future)
    , mk "kappalainen"                  (past ... future)
    , mk "yhteiskuntasihteeri"          (past ... future)
    , mk "kirjastonhoitaja"             (past ... future)
    , mk "opastussihteeri"              (past ... future)
    , mk "jäsensihteeri"                (past ... future)
    , mk "mainossihteeri"               (past ... future)
    , mk "historioitsija"               (past ... future)
    , mk "galleristi"                   (past ... future)
    , mk "valokuvaaja"                  (past ... future)
    , mk "laulunjohtaja"                (past ... future)
    , mk "tietotekniikkavastaava"       (past ... future)
    , mk "verkkovastaava"               (past ... future)
    , mk "musiikkihuoneenhoitaja"       (past ... future)
    , mk "haromäenisäntä"               (past ... future)
    , mk "valvontatilintarkastaja"      (past ... future)
    , mk "varavalvontatilintarkastaja"  (past ... future)
    , mk "seniorisihteeri"              (2017 ... future)
    , mk "juhlamestari"                 (past ... future)
    ]
  where
    mk = (,)
    future = virkailijatTo
    past   = virkailijatFrom

virkailijatFrom :: Int
virkailijatFrom = 2011

virkailijatTo :: Int
virkailijatTo = 2017

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
