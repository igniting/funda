module Funda.Backend.Bitcask.Types where

import qualified Data.ByteString   as B
import qualified Data.HashTable.IO as HT
import           Data.Word

type K   = B.ByteString
type V   = B.ByteString

-- Store offset of key
type OffsetTable = HT.BasicHashTable K Integer

-- Hint Log format
data HintLog = HintLog { hKeySize :: Word32
                       , hKey     :: K
                       , hOffset  :: Word64
                       }

-- Data Log format
data DataLog = DataLog { dKeySize   :: Word32
                       , dKey       :: K
                       , dValueSize :: Word32
                       , dValue     :: V
                       }

-- Config for the database
data Config = Config { recordsFileName :: FilePath
                     , hintFileName :: FilePath
                     }

data Bitcask k v = Bitcask { currRecords :: FilePath
                           , currHint :: FilePath
                           , offsetTable :: OffsetTable
                           }
