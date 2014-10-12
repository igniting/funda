{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Funda.Backend.Backend where

import           Funda.Backend.Serializable

class (Monad (QueryM b), Monad (UpdateM b)) => RawBackend b where
  data Key     b
  data Value   b
  data QueryM  b :: * -> *
  data UpdateM b :: * -> *
  getRaw    :: Key b -> QueryM b (Maybe (Value b))
  putRaw    :: Key b -> Value b -> UpdateM b ()
  deleteRaw :: Key b -> UpdateM b ()
  runQuery  :: QueryM b a -> b -> a
  runUpdate :: UpdateM b a -> b -> ()

class Database b k v where
  query  :: (Backend b k v) => b -> k -> Maybe v
  update :: (Backend b k v) => b -> k -> v -> ()
  delete :: (Backend' b k)  => b -> k -> ()

instance Database b k v where
  query  backend key = runQuery (get key) backend
  update backend key value = runUpdate (put key value) backend
  delete backend key = runUpdate (del key) backend

class (RawBackend b, Serializable key (Key b), Serializable value (Value b))
      => Backend b key value where
  get :: key -> QueryM b (Maybe value)
  get k = getRaw (encode k) >>= (return . fromMaybe . fmap decode )
    where
      fromMaybe :: Maybe (Either String a) -> Maybe a
      fromMaybe Nothing            = Nothing
      fromMaybe (Just (Right a))   = Just a
      fromMaybe (Just (Left err))  = error $ "Could not decode. Data probably corrupted \n" ++ err

  put :: key -> value -> UpdateM b ()
  put k v = putRaw (encode k) (encode v)

class (RawBackend b, Serializable key (Key b)) => Backend' b key where
  del :: key -> UpdateM b ()
  del k = deleteRaw $ encode k

-- instance (Backend backend, key ~ (Key backend), value ~ (Value backend))
--          => SpecializedBackend backend key value  where
--   get    = getRaw
--   put    = putRaw
--   delete = deleteRaw
