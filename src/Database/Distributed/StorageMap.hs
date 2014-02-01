{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}


module Database.Distributed.StorageMap where

import Database.Distributed.Message
import Database.Distributed.Key

import Control.Distributed.Process (Process, liftIO)

import Control.Monad.State (get, put)
import Control.Monad.Reader (ask)
import Control.Monad (void)

import Data.SafeCopy
import Data.Acid

import Data.Map (Map)
import qualified Data.Map as Map

newtype T = T { unStorageMap :: Map Key Value } deriving (Show)

$(deriveSafeCopy 0 'base ''T)


insertX :: Key -> Value -> Update T ()
insertX k v = do
  T sm <- get
  put (T $ Map.insert k v sm)

lookupX :: Key -> Query T (Maybe Value)
lookupX k = do
  T sm <- ask
  return (Map.lookup k sm)

getStorageMapX :: Query T (Map Key Value)
getStorageMapX = ask >>= return . unStorageMap

$(makeAcidic ''T ['insertX, 'lookupX, 'getStorageMapX])


empty :: T
empty = T Map.empty

insert ::
  Key ->
  Value ->
  AcidState (EventState InsertX) ->
  Process ()
insert k v db = void $ liftIO $ update db (InsertX k v)


lookup ::
  Key ->
  AcidState (EventState LookupX) ->
  Process (EventResult LookupX)
lookup k db = liftIO $ query db (LookupX k)

getStorageMap ::
  AcidState (EventState GetStorageMapX) -> Process (EventResult GetStorageMapX)
getStorageMap db = liftIO $ query db GetStorageMapX