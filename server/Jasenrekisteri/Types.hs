module Jasenrekisteri.Types where

import Servant

-- TODO: use uuid
newtype UserId = UserId Int

instance FromHttpApiData UserId where
    parseUrlPiece = fmap UserId . parseUrlPiece

getUserId :: UserId -> Int
getUserId (UserId uid) = uid
