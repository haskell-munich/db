{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.Distributed.Server where

-- UDP reparieren mit:
-- sudo route add -net 224.0.0.0 netmask 224.0.0.0 lo

import Database.Distributed.Message
import Database.Distributed.Parser
import Database.Distributed.Key
import Database.Distributed.Utility (green, yellow, reset)

import qualified Database.Distributed.StorageMap as StorageMap


import Control.Distributed.Process
import Control.Distributed.Process.Backend.SimpleLocalnet

import qualified Data.Map as Map
import qualified Data.Bimap as Bimap

import qualified Network as Net

import Text.Printf (printf, hPrintf)
import Data.Acid (AcidState)

import Control.Monad (forever, void)

import System.IO
  ( hSetNewlineMode, hSetBuffering,
    BufferMode(NoBuffering), Handle,
    hPutStrLn,
    universalNewlineMode)

{-
--pm :: ProcessMap
pm = Bimap.fromList $
  (1, 10002) :
  (2, 10001) :
  (3, 10004) :
  (4, 10003) :
  (9, 10008) :
  (10, 10005) :
  (12, 10007) :
  (18, 10006) :
  []
-}


responsibleProcess :: ProcessMap -> Key -> [ProcessId]
responsibleProcess pm (Key k) = map snd as
  where (l, g) = Map.partitionWithKey (\(Key x) _ -> x <= k) (Bimap.toMap pm)
        as = merge (map f $ Map.toDescList l)
                   (map f $ Map.toAscList g)
        f (Key a, b) = (abs (a-k), b)
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
  void $ liftIO $ printf
    ("Listening on port " ++ yellow ++ "%d" ++ reset ++ ".\n") myPort
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

           liftIO $ void $ hPrintf hdl ("recognised: " ++ show cs ++ "\n")
           mapM_ exec cs

ok :: String
ok = green ++ "Ok!" ++ reset

server ::
  Center ->
  Backend ->
  (ProcessMap, AcidState StorageMap.T) ->
  Process ()
server centerKey backend (pm, sm) = do
  m <- receiveWait $
         match database :
         match whereIsReply :
         match processMessage :
         match monitorEvent :
         match broadcastEvent :
         []
  server centerKey backend m
  where
        returnMaps pmap smap = return (pmap, smap)

        database msg@(from, (Lookup key)) = do
          let pid =
                case responsibleProcess pm key of
                     (q:_) -> q
                     _ -> error "Server.server: no responsible process found"

          mypid <- getSelfPid
          if pid /= mypid
             then send pid msg
             else do v <- StorageMap.lookup key sm
                     send from v
          returnMaps pm sm

        database msg@(from, (Insert key value)) = do
          let (p, pid) =
                case responsibleProcess pm key of
                     r@(q:_) -> (r, q)
                     _ -> error "Server.server: no responsible process found"
          liftIO $ print p
          mypid <- getSelfPid
          if pid /= mypid
             then do send pid msg
                     returnMaps pm sm
             else do send from ok
                     StorageMap.insert key value sm
                     returnMaps pm sm

        database (from, SM msg) = do
          let ks = Bimap.elems pm
          mapM_ (flip send (Broadcast msg)) ks
          send from ok
          returnMaps pm sm


        database msg = do
          liftIO $ print $ "server.database: " ++ show msg
          undefined

        broadcastEvent (Broadcast ShowCenter) = do
          liftIO $ putStrLn ("My center is " ++ show centerKey ++ ".")
          returnMaps pm sm

        broadcastEvent (Broadcast ShowStorageMap) = do
          smap <- StorageMap.getStorageMap sm
          liftIO $ print smap
          returnMaps pm sm

        broadcastEvent (Broadcast ShowProcessMap) = do
          liftIO $ print pm
          returnMaps pm sm

        whereIsReply (WhereIsReply _ (Just pid)) = do
          mypid <- getSelfPid
          send pid (AskCenter mypid centerKey)
          returnMaps pm sm

        whereIsReply msg = do
          liftIO $ print $ "whereIsReply.database " ++ show msg
          undefined

        processMessage (AskCenter pid ck) = do
          let newpm = Bimap.insert ck pid pm
          mypid <- getSelfPid
          send pid (TellCenter mypid centerKey)
          returnMaps newpm sm

        processMessage (TellCenter pid ck) = do
          let newpm = Bimap.insert ck pid pm
          void $ monitor pid
          returnMaps newpm sm

        monitorEvent (ProcessMonitorNotification _ pid _) = do
          let newpm = Bimap.deleteR pid pm
          returnMaps newpm sm




master :: AcidState StorageMap.T -> Center -> Backend -> Process ()
master acidsm centerKey backend = do
  sm <- StorageMap.getStorageMap acidsm
  liftIO $ print sm

  slaves <- liftIO $ findPeers backend 1000
  -- say $ "Slaves: " ++ show slaves

  pid <- getSelfPid
  register registerStr pid

  void $ spawnLocal $ repl pid (10000 + unKey centerKey)

  mapM_ (flip whereisRemoteAsync registerStr) slaves
  server centerKey backend (Bimap.empty, acidsm)

