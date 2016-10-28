{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module SatO.Jasenrekisteri.Session (
    -- * Login
    LoginUser (..),
    LoginData (..),
    ) where

import Prelude ()
import Futurice.Prelude

import qualified Database.PostgreSQL.Simple.ToField as P

-------------------------------------------------------------------------------
-- LoginData
-------------------------------------------------------------------------------

newtype LoginUser = LoginUser { getLoginUser :: Text }

instance IsString LoginUser where
    fromString = LoginUser . fromString

instance P.ToField LoginUser where
    toField = P.toField . getLoginUser

data LoginData = LoginData
    { loginUser :: !LoginUser
    , loginPass :: !Text
    }
