{-# LANGUAGE AutoDeriveTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
-- |
-- Module      :  Servant.Yaml
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- An @YAML@ empty data type with `MimeRender` instances for @yaml@ /
-- @aeson@'s `ToJSON` class and `Value` datatype.  You should only need to
-- import this module for it's instances and the `YAML` datatype.:
--
-- >>> type YamlGET a = Get '[YAML] a
--
-- Will then check that @a@ has a `ToJSON` instance (`Value` has).
module Servant.Xlsx where

import Codec.Xlsx.Types  (Xlsx)
import Codec.Xlsx.Writer (fromXlsx)
import Servant.API       (Accept (..), MimeRender (..))

import qualified Network.HTTP.Media as M

data XLSX -- deriving Typeable

-- | @application/x-yaml@
instance Accept XLSX where
    contentType _ = "application" M.// "vnd.openxmlformats-officedocument.spreadsheetml.sheet"

-- | `fromXlsx`
instance ToXlsx a => MimeRender XLSX a where
    mimeRender _ = fromXlsx 0 . toXlsx

{-
--  `decodeEither`
instance FromJSON a => MimeUnrender YAML a where
    mimeUnrender _ = decodeEither . LBS.toStrict
-}

-------------------------------------------------------------------------------
-- Class
-------------------------------------------------------------------------------

class ToXlsx a where
    toXlsx :: a -> Xlsx
