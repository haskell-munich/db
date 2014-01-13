{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

-- UDP reparieren mit:
-- sudo route add -net 224.0.0.0 netmask 224.0.0.0 lo

import Message
import Parser

import Data.Binary
import Data.Typeable
import GHC.Generics (Generic)

import System.Environment (getArgs)
import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node (initRemoteTable, runProcess)
import Control.Distributed.Process.Backend.SimpleLocalnet

import qualified Data.Map as Map; import Data.Map (Map)
import qualified Data.Bimap as Bimap; import Data.Bimap (Bimap)

import qualified Network as Net

import System.Random

import Text.Printf (printf, hPrintf)

import Control.Applicative (liftA2)

import Control.Monad (zipWithM_, zipWithM, mapM_, mapM, forever)
import Control.Concurrent (threadDelay, forkIO)


import System.IO
  ( hGetLine, hSetNewlineMode, hSetBuffering,
    BufferMode(LineBuffering, NoBuffering), Handle,
    hPutStrLn,
    universalNewlineMode, stdout)


--pm :: ProcessMap
pm = Map.fromList $
  (2, 10001) :
  (1, 10002) :
  (4, 10003) :
  (3, 10004) :
  (10, 10005) :
  (18, 10006) :
  (12, 10007) :
  (9, 10008) :
  []


responsibleProcess :: ProcessMap -> Key -> [ProcessId]
responsibleProcess pm k = map snd as
  where (l, g) = Map.split k (Bimap.toMap pm)
        as = merge (map f $ Map.toDescList l)
                   (map f $ Map.toAscList g)
        f (a, b) = (a-k, b)
        merge [] ys = ys
        merge xs [] = xs
        merge (x:xs) (y:ys) =
          if fst x < fst y then x : merge xs (y:ys) else y : merge (x:xs) ys


port :: Int
port = 10000

registerStr :: String
registerStr = "database"


repl :: ProcessId -> Int -> Process ()
repl pid myPort = do
  liftIO $ printf "Listening on port %d\n" myPort
  sock <- liftIO $ Net.listenOn (Net.PortNumber $ fromIntegral myPort)
  forever $ do
    (hdl, _, _) <- liftIO $ Net.accept sock
    spawnLocal $ repl2 pid hdl



repl2 :: ProcessId -> Handle -> Process ()
repl2 pid hdl = do
  liftIO $ hSetNewlineMode hdl universalNewlineMode
  liftIO $ hSetBuffering hdl NoBuffering
  mypid <- getSelfPid
  forever $ do
    cmd <- liftIO $ prompt hdl
    case cmd of
         Nothing -> return ()
         Just [] -> return ()
         Just cs -> do
           let exec c = do
                 send pid (mypid, c)
                 res <- receiveWait [
                   match $ (return . show :: Maybe String -> Process String),
                   match $ return ]
                 liftIO $ hPutStrLn hdl res

           liftIO $ hPrintf hdl ("recognised: " ++ show cs ++ "\n")
           mapM_ exec cs


remotable []


server :: Center -> Backend -> (ProcessMap, StorageMap) -> Process ()
server centerKey backend (pm, sm) = do
  -- liftIO $ print pm
  liftIO $ print centerKey
  liftIO $ print sm
  m <- receiveWait $
         match database :
         match whereIsReply :
         match processMessage :
         match monitorEvent :
         []
  server centerKey backend m
  where 
        database msg@(from, (Lookup key)) = do
          let (pid:_) = responsibleProcess pm key
          mypid <- getSelfPid
          if pid /= mypid
             then send pid msg
             else send from (Map.lookup key sm)
          return (pm, sm)

        database msg@(from, (Insert key value)) = do
          let p@(pid:_) = responsibleProcess pm key
          liftIO $ print p
          mypid <- getSelfPid
          if pid /= mypid
             then do send pid msg
                     return (pm, sm)
             else do send from "Insert ok!"
                     let newsm = Map.insert key value sm
                     return (pm, newsm)



        whereIsReply (WhereIsReply str (Just pid)) = do
          mypid <- getSelfPid
          send pid (AskCenter mypid centerKey)
          return (pm, sm)

        processMessage (AskCenter pid ck) = do
          let newpm = Bimap.insert ck pid pm
          mypid <- getSelfPid
          send pid (TellCenter mypid centerKey)
          return (newpm, sm)

        processMessage (TellCenter pid ck) = do
          let newpm = Bimap.insert ck pid pm
          monitor pid
          return (newpm, sm)

        monitorEvent (ProcessMonitorNotification _ pid _) = do
          let newpm = Bimap.deleteR pid pm
          return (newpm, sm)



master :: Backend -> Process ()
master backend = do
  slaves <- liftIO $ findPeers backend 1000
  centerKey <- liftIO (getStdRandom (randomR (0, 1000 :: Int)))
  say $ "Slaves: " ++ show slaves

  pid <- getSelfPid
  register registerStr pid

  spawnLocal $ repl pid (10000+centerKey)

  mapM_ (flip whereisRemoteAsync registerStr) slaves
  server centerKey backend (Bimap.empty, Map.empty)



main :: IO ()
main = do
  [host, port] <- getArgs
  backend <- initializeBackend host port (__remoteTable initRemoteTable)
  node <- newLocalNode backend
  runProcess node (master backend)
