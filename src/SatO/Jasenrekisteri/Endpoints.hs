{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE UndecidableInstances   #-}
module SatO.Jasenrekisteri.Endpoints (
    QueryM,
    queryEndpoint,
    ask,
    ) where

import Prelude ()
import Futurice.Prelude
import Control.Monad.Reader (Reader, ask, runReader)
import Servant              (Handler)

import SatO.Jasenrekisteri.Ctx   (Ctx, ctxReadWorld)
import SatO.Jasenrekisteri.World (World)

type QueryM = Reader (World, Day)

class Query arg res | arg -> res where
    queryEndpoint :: Ctx -> arg -> res

instance Query (Reader (World, Day) a) (Handler a) where
    queryEndpoint ctx r = liftIO $ do
        world <- ctxReadWorld ctx
        today <- currentDay
        pure $ runReader r (world, today)

instance Query arg res => Query (a -> arg) (a -> res) where
    queryEndpoint ctx r x = queryEndpoint ctx (r x)
