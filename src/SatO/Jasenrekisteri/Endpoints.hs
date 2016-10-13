{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE UndecidableInstances   #-}
module SatO.Jasenrekisteri.Endpoints where

import Control.Monad.Reader (Reader, runReader)
import Futurice.Prelude
import Prelude ()
import Servant              (Handler)

import SatO.Jasenrekisteri.Ctx   (Ctx, ctxReadWorld)
import SatO.Jasenrekisteri.World (World)

type QueryM = Reader World

class Query arg res | arg -> res where
    queryEndpoint :: Ctx -> arg -> res

instance Query (Reader World a) (Handler a) where
    queryEndpoint ctx r = liftIO $ do
        world <- ctxReadWorld ctx
        pure $ runReader r world

instance Query arg res => Query (a -> arg) (a -> res) where
    queryEndpoint ctx r x = queryEndpoint ctx (r x)
