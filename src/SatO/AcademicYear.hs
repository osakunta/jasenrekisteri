module SatO.AcademicYear where

import Prelude ()
import Futurice.Prelude
import Control.Lens     (to)
import Data.Time        (fromGregorian, toGregorian)

-- | Get academic year at that day.
--
-- We approximate academic years changing at 15th of July.
academicYear :: Day -> Integer
academicYear today
    | today > midJuly = thisYear
    | otherwise = thisYear - 1
  where
    thisYear :: Integer
    thisYear = today ^. to toGregorian . _1

    midJuly :: Day
    midJuly = fromGregorian thisYear 7 15
