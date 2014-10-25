{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Funda.Backend.Bitcask.Bitcask where

import           Control.Monad.Reader         as Reader
import qualified Control.Monad.State          as State
import qualified Data.Aeson                   as Aeson
import qualified Data.ByteString              as B

import           Funda.Backend.Backend
import           Funda.Backend.Bitcask.Memory
import           Funda.Backend.Bitcask.Types  as Types

instance Monad (QueryM (Bitcask Types.K Types.V)) where
  return  = BitcaskQuery . return
  (BitcaskQuery r) >>= f = BitcaskQuery (r >>= unQuery . f)

instance Monad (UpdateM (Bitcask Types.K Types.V) ) where
  return = BitcaskUpdate . return
  (BitcaskUpdate r) >>= f = BitcaskUpdate (r >>= unUpdate . f)

instance Backend (Bitcask Types.K Types.V) where
  data Key     (Bitcask Types.K Types.V) = BitcaskK Types.K
  data Value   (Bitcask Types.K Types.V) = BitcaskV Types.V
  data QueryM  (Bitcask Types.K Types.V) a =
    BitcaskQuery { unQuery :: Reader.Reader (Bitcask Types.K Types.V) a}
  data UpdateM (Bitcask Types.K Types.V) a =
    BitcaskUpdate { unUpdate :: State.State  (Bitcask Types.K Types.V) a}
  get = undefined
  put = undefined
  del = undefined
  runQuery = undefined
  runUpdate = undefined

openJSONBitcask :: String -> Bitcask B.ByteString Aeson.Value
openJSONBitcask path = Bitcask { keyDir = openMemoryMap path
                               , dataDir = path
                               , settings = defaultSettings
                               }
