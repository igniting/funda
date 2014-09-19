module Funda.Backend.Bitcask.Get where

import Data.ByteString

import Funda.Backend.Bitcask.Types

get :: Bitcask -> Key -> IO Value
get _ _ = do
  return empty
