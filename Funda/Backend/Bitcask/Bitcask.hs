{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Funda.Backend.Bitcask.Bitcask where

import           Control.Monad.State.Lazy
import qualified Data.Aeson               as Aeson
import qualified Data.ByteString          as B
import           Data.ByteString.Lazy     (empty)
import           Data.Coerce

import Funda.Backend.Backend
import Funda.Backend.Bitcask.Memory
import Funda.Backend.Bitcask.Types  as Types
import Funda.Backend.Serializable

type JSONBitcask =  Bitcask B.ByteString Aeson.Value
type RawBitcask = Bitcask Types.K Types.V

instance Backend RawBitcask where
  type Key     RawBitcask = Types.K
  type Value   RawBitcask = Types.V
  query = undefined
  update k _ = do
    bitcask <- get
    return $ do
      print k
      return ()
  del = undefined

instance Database JSONBitcask RawBitcask where
  type V JSONBitcask = Aeson.Value
  type K JSONBitcask = B.ByteString
  toBackend = coerce
  toDatabase = coerce

openJSONBitcask :: String -> JSONBitcask
openJSONBitcask path = Bitcask { keyDir = openMemoryMap path
                               , dataDir = path
                               , settings = defaultSettings
                               }

instance Serializable Aeson.Value Types.V where
  decode _ = Right Aeson.Null
  encode _ = empty

main :: IO ()
main = runUpdate run $ openJSONBitcask "/dev/null"

run :: Update JSONBitcask (IO ())
run = do
  insert B.empty Aeson.Null
  insert B.empty Aeson.Null
  fmap void (liftQuery $ find B.empty)
  delete B.empty
