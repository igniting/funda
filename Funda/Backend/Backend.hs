{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
module Funda.Backend.Backend where

class Monad (BackendMonad b) =>  Backend b where
  type Key   b
  type Value b
  type BackendMonad b :: * -> *
  get    :: b -> Key b -> BackendMonad b (Value b)
  put    :: b -> Key b -> Value b -> BackendMonad b ()
  delete :: b -> Key b -> BackendMonad b ()
