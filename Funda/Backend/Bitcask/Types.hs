module Funda.Backend.Bitcask.Types where

import           Control.Concurrent.STM.TVar
import qualified Data.ByteString             as B
import qualified Data.ByteString.Lazy        as LazyB
import           Data.HashMap.Strict         (HashMap)

type Key = B.ByteString
type Value = LazyB.ByteString

type KeyDir = HashMap Key Value

data BitcaskSettings =  BitcaskSettings { maxBytes :: Integer
                                        } deriving (Show, Eq)

data Bitcask =  Bitcask { keyDir   :: TVar KeyDir
                        , dataDir  :: FilePath
                        , settings :: BitcaskSettings
                        }

tombstone :: Value
tombstone = LazyB.empty
