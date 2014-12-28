module Funda.Backend.Bitcask.Memory where

import Funda.Backend.Bitcask.Types

import qualified Data.Map   as Map
import           Data.Maybe
import           Prelude    hiding (lookup)

insertMem :: MemoryMap -> K -> V -> MemoryMap
insertMem m key val = Map.insert key val m

queryMem :: MemoryMap -> K -> Maybe V
queryMem m key = Map.lookup key m

deleteMem :: MemoryMap -> K ->  MemoryMap
deleteMem m key = Map.delete key m

openMemoryMap :: String -> MemoryMap
openMemoryMap _ = openEmptyMemoryMap

openEmptyMemoryMap :: MemoryMap
openEmptyMemoryMap = Map.empty
