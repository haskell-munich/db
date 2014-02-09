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
import qualified Data.List as List

import Test.QuickCheck

type Value = String

class Command x where
  command :: x -> String

data Message = Lookup Key
             | Insert Key Value
             | Delete Key
             | SM ShowMessage deriving (Typeable, Generic, Show)

instance Binary Message

instance Arbitrary Message where
  arbitrary = do
    x <- choose (0, 2 :: Int)
    key <- arbitrary
    n <- choose (1, 10 :: Int)
    str <- vectorOf n (choose ('a', 'z'))
    return $ case x of
         0 -> Lookup key
         1 -> Insert key str
         2 -> Delete key
         _ -> error "arbitrary Message: not reachable"

instance Command Message where
  command (Lookup k) = "look " ++ show (unKey k) ++ ";"
  command (Insert k v) = "ins " ++ show (unKey k) ++ " " ++ v ++ ";"
  command (Delete k) = "del " ++ show (unKey k) ++ ";"
  command _ = "command Message: not reachable"

instance (Command a) => Command [a] where
  command = List.intercalate " " . map command


data ProcessMessage =
  AskCenter ProcessId Center
  | TellCenter ProcessId Center deriving (Typeable, Generic, Show)

instance Binary ProcessMessage

data ShowMessage =
  ShowCenter
  | ShowStorageMap
  | ShowProcessMap
  | ShowStorageSize deriving (Typeable, Generic, Show)

instance Binary ShowMessage

-- instance Arbitrary ShowMessage where

instance Command ShowMessage where
  command ShowCenter = "show cen;"
  command ShowStorageMap = "show sto;"
  command ShowProcessMap = "show pro;"
  command ShowStorageSize = "show size;"



data Broadcast = Broadcast ShowMessage deriving (Typeable, Generic, Show)

instance Binary Broadcast

type ProcessMap = Bimap Center ProcessId

