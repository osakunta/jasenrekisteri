module SatO.Jasenrekisteri.Ctx where

import Control.Concurrent.STM
       (TVar, atomically, modifyTVar', newTVarIO, readTVarIO)

import SatO.Jasenrekisteri.Command
import SatO.Jasenrekisteri.World

data Ctx = Ctx
    { ctxWorld :: TVar World
    }

ctxReadWorld :: Ctx -> IO World
ctxReadWorld = readTVarIO . ctxWorld

newCtx :: World -> IO Ctx
newCtx w = Ctx <$> newTVarIO w

-- TODO: log
ctxApplyCmd :: Command -> Ctx -> IO ()
ctxApplyCmd cmd ctx = do
    print cmd
    atomically $ modifyTVar' (ctxWorld ctx) (applyCommand cmd)
