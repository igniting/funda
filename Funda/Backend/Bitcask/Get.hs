module Funda.Backend.Bitcask.Get where

import           Funda.Backend.Bitcask.Types

get :: Bitcask -> Key -> IO (Maybe Value)
get _ _ = do
  return Nothing
