module SatO.Jasenrekisteri.Ctx where

import Control.Concurrent.STM
       (TVar, atomically, modifyTVar', newTVarIO, readTVarIO)
import Crypto.Random          (newGenIO)
import Crypto.Random.DRBG     (HmacDRBG)
import Data.Pool              (Pool, createPool)

import SatO.Jasenrekisteri.Command
import SatO.Jasenrekisteri.World

data Ctx = Ctx
    { ctxWorld :: TVar World
    , ctxPRNGs :: Pool (TVar HmacDRBG)
    }

ctxReadWorld :: Ctx -> IO World
ctxReadWorld = readTVarIO . ctxWorld

newCtx :: World -> IO Ctx
newCtx w = Ctx
    <$> newTVarIO w
    <*> createPool (newGenIO >>= newTVarIO) (\_ -> return()) 1 3600 5

-- TODO: log
ctxApplyCmd :: Command -> Ctx -> IO ()
ctxApplyCmd cmd ctx = do
    print cmd
    atomically $ modifyTVar' (ctxWorld ctx) (applyCommand cmd)
