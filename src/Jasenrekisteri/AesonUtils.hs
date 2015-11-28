module Jasenrekisteri.AesonUtils (
    recordOptions,
    ) where

import Data.Aeson.TH (Options (..), defaultOptions)
import Data.Char     (toLower)

lowerFirst :: String -> String
lowerFirst (c:cs) = toLower c : cs
lowerFirst []     = []

recordOptions :: Int -> Options
recordOptions n = defaultOptions
    { fieldLabelModifier = lowerFirst . drop n
    }
