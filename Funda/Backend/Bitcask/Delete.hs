module Funda.Backend.Bitcask.Delete where

import           Funda.Backend.Bitcask.Put
import           Funda.Backend.Bitcask.Types

delete :: Bitcask -> Key -> IO ()
delete b k = put b k tombstone
