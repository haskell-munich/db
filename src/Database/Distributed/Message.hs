{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}


module Database.Distributed.Message where


import Data.Binary
import Data.Typeable
import GHC.Generics (Generic)

import Control.Distributed.Process (ProcessId)

import Data.Map (Map)
import Data.Bimap (Bimap)


type Key = Int
type Center = Key
type Value = String

data Message = Lookup Key
             | Insert Key Value
             | Delete Key deriving (Typeable, Generic, Show)

instance Binary Message

type StorageMap = Map Key Value



data ProcessMessage =
  AskCenter ProcessId Center
  | TellCenter ProcessId Center deriving (Typeable, Generic, Show)

instance Binary ProcessMessage

type ProcessMap = Bimap Center ProcessId



