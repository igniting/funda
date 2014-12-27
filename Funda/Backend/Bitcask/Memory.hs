module Funda.Backend.Bitcask.Memory where

import Funda.Backend.Bitcask.Types

import           Control.Monad.State
import qualified Data.Map            as Map
import           Data.Maybe
import           Prelude             hiding (lookup)

insert_ :: K -> V -> State (Bitcask K V) ()
insert_ key val = do
  b@(Bitcask m _ _) <- get
  put $ b { keyDir = Map.insert key val m }

lookup :: K -> State (Bitcask K V) (Maybe V)
lookup key = do
  Bitcask m _ _ <- get
  return (Map.lookup key m)

remove :: K ->  State (Bitcask K V) ()
remove key = do
  b@(Bitcask m _ _) <- get
  put $ b { keyDir = Map.delete key m }

openMemoryMap :: String -> MemoryMap
openMemoryMap = undefined

openEmptyMemoryMap :: MemoryMap
openEmptyMemoryMap = Map.empty
