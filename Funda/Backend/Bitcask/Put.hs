module Funda.Backend.Bitcask.Put where

import           Funda.Backend.Bitcask.Types

put :: Bitcask -> Key -> Value -> IO ()
put _ _ _ = return ()
