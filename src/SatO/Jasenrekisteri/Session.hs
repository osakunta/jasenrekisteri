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

-------------------------------------------------------------------------------
-- LoginData
-------------------------------------------------------------------------------

newtype LoginUser = LoginUser { getLoginUser :: Text }

instance IsString LoginUser where
    fromString = LoginUser . fromString

data LoginData = LoginData
    { loginUser :: !LoginUser
    , loginPass :: !Text
    }
