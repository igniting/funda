{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
module Funda.Backend.Bitcask.Bitcask where

import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Aeson                 as Aeson
import qualified Data.Aeson.Encode          as AesonEncode
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy.Char8 as C
import           Data.Coerce

import Funda.Backend.Backend
import Funda.Backend.Bitcask.Memory
import Funda.Backend.Bitcask.Types  as Types
import Funda.Backend.Serializable

type JSONBitcask =  Bitcask B.ByteString String
type RawBitcask = Bitcask Types.K Types.V

instance Backend RawBitcask where
  type Key     RawBitcask = Types.K
  type Value   RawBitcask = Types.V
  query k = do
    bitcask <- ask
    return $ return $ queryMem (keyDir bitcask) k
  update k v = do
    bitcask <- get
    put $ bitcask { keyDir = insertMem (keyDir bitcask) k v }
    return $ return ()
  del k = do
    bitcask <- get
    put $ bitcask { keyDir = deleteMem (keyDir bitcask) k }
    return $ return ()

instance Database JSONBitcask RawBitcask where
  type V JSONBitcask = String -- Aeson.Value
  type K JSONBitcask = B.ByteString
  toBackend = coerce
  toDatabase = coerce


openRawBitcask :: String -> RawBitcask
openRawBitcask path = Bitcask { keyDir = openMemoryMap path
                              , dataDir = path
                              , settings = defaultSettings
                              }

openJSONBitcask :: String -> JSONBitcask
openJSONBitcask = coerce . openRawBitcask

instance Serializable String Types.V where
  decode = Right . C.unpack -- Aeson.eitherDecode
  encode = C.pack

main :: IO ()
main = runUpdate run $ openJSONBitcask "/dev/null"

run :: Update JSONBitcask (IO ())
run = do
  insert "foo" "FOO"
  insert "bar" "BAR"
  fmap (>>= print) (liftQuery $ find "foo")
  delete "foo"
  fmap (>>= print) (liftQuery $ find "foo")
