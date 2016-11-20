{-# LANGUAGE OverloadedStrings #-}
module SatO.Jasenrekisteri.Ctx where

import Prelude ()
import Futurice.Prelude
import Control.Concurrent.STM
       (TVar, atomically, modifyTVar', newTVarIO, readTVarIO)
import Crypto.Random          (newGenIO)
import Crypto.Random.DRBG     (HmacDRBG)
import Data.Pool              (Pool, createPool, withResource)

import qualified Database.PostgreSQL.Simple as P

import SatO.Jasenrekisteri.Command
import SatO.Jasenrekisteri.Session
import SatO.Jasenrekisteri.Person
import SatO.Jasenrekisteri.World

data Ctx = Ctx
    { ctxWorld     :: TVar World
    , ctxOrigWorld :: World
    , ctxPostgres  :: Pool P.Connection
    , ctxPRNGs     :: Pool (TVar HmacDRBG)
    }

ctxReadWorld :: Ctx -> IO World
ctxReadWorld = readTVarIO . ctxWorld

newCtx :: P.ConnectInfo -> World -> IO Ctx
newCtx ci w = Ctx
    <$> newTVarIO w
    <*> pure w
    <*> createPool (P.connect ci) P.close 1 60 5
    <*> createPool (newGenIO >>= newTVarIO) (\_ -> return()) 1 3600 5

-- TODO: log
ctxApplyCmd :: LoginUser -> Command I -> Ctx -> IO ()
ctxApplyCmd lu cmd ctx = do
    print cmd
    atomically $ modifyTVar' (ctxWorld ctx) (applyCommand cmd)
    withResource (ctxPostgres ctx) $ \conn -> do
        _ <- P.execute conn "INSERT INTO jasen2.events (username, edata) VALUES (?, ?)" (lu, cmd)
        -- | TODO: log what happened?
        pure ()

ctxFetchCmds :: Ctx -> PersonId -> IO [(LoginUser, UTCTime, Command I)]
ctxFetchCmds ctx memberId = withResource (ctxPostgres ctx) $ \conn -> do
    P.query conn "SELECT username, updated, edata FROM jasen2.events WHERE edata :: json ->> 'memberId' = ? ORDER by eid DESC" (P.Only memberId)

ctxFetchAllCmds :: Ctx -> Maybe CID -> IO [(CID, LoginUser, UTCTime, Command I)]
ctxFetchAllCmds ctx Nothing = withResource (ctxPostgres ctx) $ \conn -> do
    P.query_ conn "SELECT eid, username, updated, edata FROM jasen2.events ORDER by eid DESC LIMIT 100"
ctxFetchAllCmds ctx (Just cid) = withResource (ctxPostgres ctx) $ \conn -> do
    P.query conn "SELECT eid, username, updated, edata FROM jasen2.events WHERE eid < ? ORDER by eid DESC LIMIT 100" (P.Only cid)
