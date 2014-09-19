module Funda.Backend.Bitcask.Types where

import Control.Concurrent.STM.TVar
import Data.ByteString (ByteString, empty)
import Data.HashMap.Strict (HashMap)

type Key = ByteString
type Value = ByteString

type KeyDir = HashMap Key Value

data BitcaskSettings =  BitcaskSettings {
  maxBytes :: Integer
  } deriving (Show, Eq)

data Bitcask =  Bitcask {
  keyDir :: TVar KeyDir
  , dataDir :: FilePath
  , settings :: BitcaskSettings
  }

tombstone :: Value
tombstone = empty
