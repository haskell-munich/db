{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}


module Database.Distributed.Key where

import Data.Binary (Binary)
import Data.Typeable
import GHC.Generics (Generic)

import Control.Monad.Reader (ask)
import Control.Monad.State (put)

import Data.SafeCopy
import Data.Acid

newtype Key = Key { unKey :: Int } deriving (Typeable, Generic, Show, Eq, Ord)
instance Binary Key

type Center = Key

$(deriveSafeCopy 0 'base ''Key)

type MaybeCenter = Maybe Center

getCenterX :: Query MaybeCenter MaybeCenter
getCenterX = ask

setCenterX :: Int -> Update MaybeCenter ()
setCenterX x = put (Just (Key x))


$(makeAcidic ''MaybeCenter ['getCenterX, 'setCenterX])


getCenter ::
  AcidState (EventState GetCenterX) -> IO (EventResult GetCenterX)
getCenter db = query db GetCenterX


setCenter ::
  Int -> AcidState (EventState GetCenterX) -> IO ()
setCenter k db = update db (SetCenterX k)