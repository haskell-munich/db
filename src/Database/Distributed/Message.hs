{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}


module Database.Distributed.Message where

import Database.Distributed.Key

import Data.Binary (Binary)
import Data.Typeable
import GHC.Generics (Generic)

import Control.Distributed.Process (ProcessId)

import Data.Bimap (Bimap)

type Value = String

data Message = Lookup Key
             | Insert Key Value
             | Delete Key
             | SM ShowMessage deriving (Typeable, Generic, Show)

instance Binary Message




data ProcessMessage =
  AskCenter ProcessId Center
  | TellCenter ProcessId Center deriving (Typeable, Generic, Show)

instance Binary ProcessMessage

data ShowMessage =
  ShowCenter
  | ShowStorageMap
  | ShowProcessMap deriving (Typeable, Generic, Show)

instance Binary ShowMessage

data Broadcast = Broadcast ShowMessage deriving (Typeable, Generic, Show)

instance Binary Broadcast

type ProcessMap = Bimap Center ProcessId

