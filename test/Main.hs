{-# LANGUAGE TemplateHaskell #-}


module Main where


import Database.Distributed.Server
import Database.Distributed.Key

import qualified Database.Distributed.StorageMap as StorageMap

import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node (initRemoteTable, runProcess)
import Control.Distributed.Process.Backend.SimpleLocalnet

import System.Environment (getArgs)
import System.Directory (createDirectoryIfMissing)
import System.FilePath.Posix ((</>))
import System.Random

import Data.Acid (openLocalStateFrom)

remotable []

host :: String
host = "localhost"

dbdir :: String
dbdir = "database"


main :: IO ()
main = do
  [theport] <- getArgs
  createDirectoryIfMissing True dbdir

  let dir = dbdir </> theport
  database <- openLocalStateFrom (dir </> "storagemap") StorageMap.empty



  centerdb <- openLocalStateFrom (dir </> "center") (Nothing :: MaybeCenter)

  key <- getCenter centerdb
  thecenter <-
    case key of
         Nothing -> do
           center <- getStdRandom (randomR (0, 1000 :: Int))
           setCenter center centerdb
           return (Key center)
         Just tc -> return tc


  putStrLn ("Starting with center " ++ show thecenter)


  backend <- initializeBackend host theport (__remoteTable initRemoteTable)
  node <- newLocalNode backend
  runProcess node (master database thecenter backend)
