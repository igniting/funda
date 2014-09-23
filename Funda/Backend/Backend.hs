{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Funda.Backend.Backend where

import           Funda.Backend.Serializable

class (Monad (QueryM b), Monad (UpdateM b)) =>  Backend b where
  type Key     b
  type Value   b
  type QueryM  b :: * -> *
  type UpdateM b :: * -> *
  getRaw    :: b -> Key b -> QueryM b (Maybe (Value b))
  putRaw    :: b -> Key b -> Value b -> UpdateM b ()
  deleteRaw :: b -> Key b -> UpdateM b ()

class (Backend b, Serializable key (Key b), Serializable value (Value b))
      => SpecializedBackend b key value where
  get :: b -> key -> QueryM b (Maybe value)
  get backend k = getRaw backend (encode k) >>= (return . fromMaybe . fmap decode )
    where
      fromMaybe :: Maybe (Either String a) -> Maybe a
      fromMaybe Nothing            = Nothing
      fromMaybe (Just (Right a))   = Just a
      fromMaybe (Just (Left err))  = error $ "Could not decode. Data probably corrupted \n" ++ err

  put :: b -> key -> value -> UpdateM b ()
  put backend k v = putRaw backend (encode k) (encode v)

  delete :: b -> key -> UpdateM b ()
  delete backend k = deleteRaw backend $ encode k

instance (Backend backend, key ~ (Key backend), value ~ (Value backend))
         => SpecializedBackend backend key value  where
  get    = getRaw
  put    = putRaw
  delete = deleteRaw
