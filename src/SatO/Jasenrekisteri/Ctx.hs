module SatO.Jasenrekisteri.Ctx where

import Control.Concurrent.STM
       (TVar, atomically, modifyTVar', newTVarIO, readTVarIO)
import Crypto.Random          (newGenIO)
import Crypto.Random.DRBG     (HmacDRBG)
import Data.Pool              (Pool, createPool)

import qualified Database.PostgreSQL.Simple as Postgres

import SatO.Jasenrekisteri.Command
import SatO.Jasenrekisteri.World

data Ctx = Ctx
    { ctxWorld    :: TVar World
    , ctxPostgres :: Pool Postgres.Connection
    , ctxPRNGs    :: Pool (TVar HmacDRBG)
    }

ctxReadWorld :: Ctx -> IO World
ctxReadWorld = readTVarIO . ctxWorld

newCtx :: Postgres.ConnectInfo -> World -> IO Ctx
newCtx ci w = Ctx
    <$> newTVarIO w
    <*> createPool (Postgres.connect ci) Postgres.close 1 60 5
    <*> createPool (newGenIO >>= newTVarIO) (\_ -> return()) 1 3600 5

-- TODO: log
ctxApplyCmd :: Command -> Ctx -> IO ()
ctxApplyCmd cmd ctx = do
    print cmd
    atomically $ modifyTVar' (ctxWorld ctx) (applyCommand cmd)
