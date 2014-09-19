module Funda.Backend.Bitcask.Delete where

import Funda.Backend.Bitcask.Types
import Funda.Backend.Bitcask.Put

delete :: Bitcask -> Key -> IO ()
delete b k = put b k tombstone
