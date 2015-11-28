{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Prelude        ()
import Prelude.Compat

import Data.Char          (ord)
import Data.List          (sortBy)
import Data.Monoid        ((<>))
import Data.Ord           (comparing)
import Data.Vector        (Vector)
import System.Environment (getArgs)

import qualified Data.Aeson.Compat       as A
import qualified Data.ByteString.Lazy    as LBS
import qualified Data.Csv                as Csv
import qualified Data.Text.Lazy.Encoding as LTE
import qualified Data.Vector             as V

import Jasenrekisteri.Person

readAscii :: FilePath -> IO LBS.ByteString
readAscii filepath = do
    contents <- LBS.readFile filepath
    pure . LTE.encodeUtf8 . LTE.decodeLatin1 $ contents

run :: FilePath -> FilePath -> IO ()
run filepathIn filepathOut = do
    contents <- readAscii filepathIn
    let d = Csv.decodeWith decOptions Csv.HasHeader contents :: Either String (Vector Person)
    case d of
        Left err -> putStrLn err
        Right members -> do
            let members' = V.fromList
                         . sortBy (comparing personFirstNames <> comparing personLastName)
                         . V.toList
                         $ members
            putStrLn $ "Read members: " ++ show (V.length members')
            print $ V.take 20 members'
            LBS.writeFile filepathOut $ A.encode members'
  where
    decOptions = Csv.defaultDecodeOptions {
        Csv.decDelimiter = fromIntegral (ord ';')
    }

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filepathIn, filepathOut] -> run filepathIn filepathOut
        _ -> putStrLn "Usage: ./excel excel-export.csv data.json"
