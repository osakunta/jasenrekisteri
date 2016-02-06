module Jasenrekisteri.Types where

import Servant

newtype UserId = UserId Int

instance FromText UserId where
    fromText = fmap UserId . fromText

getUserId :: UserId -> Int
getUserId (UserId uid) = uid
