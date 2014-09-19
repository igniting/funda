{-# LANGUAGE TypeFamilies #-}
module Funda.Backend.Bitcask.Bitcask where

import Funda.Backend.Backend
import Funda.Backend.Bitcask.Get as BitcaskGet
import Funda.Backend.Bitcask.Put as BitcaskPut
import Funda.Backend.Bitcask.Delete as BitcaskDelete
import Funda.Backend.Bitcask.Types as BitcaskTypes

instance Backend Bitcask where
  type Key   Bitcask = BitcaskTypes.Key
  type Value Bitcask = BitcaskTypes.Value
  get = BitcaskGet.get
  put = BitcaskPut.put
  delete = BitcaskDelete.delete
