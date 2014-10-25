{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
module Funda.Backend.Backend where

import           Funda.Backend.Serializable

class (Monad (QueryM b), Monad (UpdateM b)) => Backend b where
  data Key     b
  data Value   b
  data QueryM  b :: * -> *
  data UpdateM b :: * -> *
  get    :: Key b -> QueryM b (Maybe (Value b))
  put    :: Key b -> Value b -> UpdateM b ()
  del    :: Key b -> UpdateM b ()
  runQuery  :: QueryM b a -> b -> IO a
  runUpdate :: UpdateM b a -> b -> IO ()

class Database b k v where
  query  :: b -> k -> IO (Maybe v)
  update :: b -> k -> v -> IO ()
  delete :: b -> k -> IO ()

-- instance (Backend b, k ~ (Key b), v ~ (Value b)) => Database b k v where
--   query  backend key = runQuery (get key) backend
--   update backend key value = runUpdate (put key value) backend
--   delete backend key = runUpdate (del key) backend

instance (Backend b, Serializable k (Key b), Serializable v (Value b))
         => Database b k v where
  query backend key = do
    val <- runQuery (get (encode  key)) backend
    return $ fromMaybeEither $fmap decode val
    where
      fromMaybeEither :: Maybe (Either String a) -> Maybe a
      fromMaybeEither Nothing            = Nothing
      fromMaybeEither (Just (Right a))   = Just a
      fromMaybeEither (Just (Left err))  = error $ "Could not decode: \n" ++ err
  update backend key value = runUpdate (put (encode key) (encode value)) backend
  delete backend key = runUpdate (del (encode key)) backend
