{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Futurice.Prelude
import Prelude ()

import System.Environment (getArgs)

import qualified Data.Aeson.Compat          as A
import qualified Data.ByteString.Lazy.Extra as LBS
import qualified Data.Csv.Extra             as Csv

import SatO.Jasenrekisteri.Member

run :: FilePath -> FilePath -> IO ()
run filepathIn filepathOut = do
    contents <- LBS.readFileAscii filepathIn
    members <- Csv.decodeSmart contents :: IO (Vector Member)
    LBS.writeFile filepathOut $ A.encode members

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filepathIn, filepathOut] -> run filepathIn filepathOut
        _ -> putStrLn "Usage: ./excel excel-export.csv data.json"
