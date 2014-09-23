{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Funda.Backend.Bitcask.Bitcask where

import           Data.ByteString.Lazy.Char8
import           Text.JSON                    as JSON

import           Funda.Backend.Backend
import           Funda.Backend.Bitcask.Delete as BitcaskDelete
import           Funda.Backend.Bitcask.Get    as BitcaskGet
import           Funda.Backend.Bitcask.Put    as BitcaskPut
import           Funda.Backend.Bitcask.Types  as BitcaskTypes
import           Funda.Backend.Serializable

instance Backend Bitcask where
  type Key     Bitcask = BitcaskTypes.Key
  type Value   Bitcask = BitcaskTypes.Value
  type QueryM  Bitcask = IO
  type UpdateM Bitcask = IO
  getRaw = BitcaskGet.get
  putRaw = BitcaskPut.put
  deleteRaw = BitcaskDelete.delete

instance Serializable JSValue BitcaskTypes.Value where
  encode = pack . JSON.encode
  decode a = case JSON.decode $ unpack a of
    JSON.Ok val -> Right val
    JSON.Error err -> Left err

instance SpecializedBackend Bitcask BitcaskTypes.Key JSON.JSValue
