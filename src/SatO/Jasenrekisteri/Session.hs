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
import Lucid            (ToHtml (..))

import qualified Database.PostgreSQL.Simple.FromField as P
import qualified Database.PostgreSQL.Simple.ToField   as P

-------------------------------------------------------------------------------
-- LoginData
-------------------------------------------------------------------------------

newtype LoginUser = LoginUser { getLoginUser :: Text }
  deriving (Show)

instance IsString LoginUser where
    fromString = LoginUser . fromString

instance P.ToField LoginUser where
    toField = P.toField . getLoginUser

instance P.FromField LoginUser where
    fromField f mdata = LoginUser <$> P.fromField f mdata

instance ToHtml LoginUser where
    toHtmlRaw = toHtmlRaw . getLoginUser
    toHtml    = toHtml . getLoginUser

data LoginData = LoginData
    { loginUser :: !LoginUser
    , loginPass :: !Text
    }
