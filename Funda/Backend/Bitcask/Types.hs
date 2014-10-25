module Funda.Backend.Bitcask.Types where

import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as LazyB
import qualified Data.Map             as Map

type K = B.ByteString
type V = LazyB.ByteString

type MemoryMap = Map.Map K V

data BitcaskSettings =  BitcaskSettings { maxBytes :: Integer
                                        } deriving (Show, Eq)

data Bitcask k v =  Bitcask { keyDir   :: MemoryMap
                            , dataDir  :: FilePath
                            , settings :: BitcaskSettings
                            }

defaultSettings :: BitcaskSettings
defaultSettings = BitcaskSettings { maxBytes = 512 }

tombstone :: V
tombstone = LazyB.empty
