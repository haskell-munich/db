{-# LANGUAGE TemplateHaskell #-}


module Main where


import Database.Distributed.Server

import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node (initRemoteTable, runProcess)
import Control.Distributed.Process.Backend.SimpleLocalnet

import System.Environment (getArgs)



remotable []

host :: String
host = "localhost"

main :: IO ()
main = do
  [theport] <- getArgs
  backend <- initializeBackend host theport (__remoteTable initRemoteTable)
  node <- newLocalNode backend
  runProcess node (master backend)
