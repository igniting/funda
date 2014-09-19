{-# LANGUAGE TypeFamilies #-}
module Funda.Backend.Backend where

class Backend b where
  type Key   b
  type Value b
  get    :: b -> Key b -> IO (Value b)
  put    :: b -> Key b -> Value b -> IO ()
  delete :: b -> Key b -> IO ()
