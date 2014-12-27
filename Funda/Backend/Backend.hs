{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
module Funda.Backend.Backend where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State

import Funda.Backend.Serializable

-- | Context monad for Update events.
newtype Update st a = Update { unUpdate :: State st a }
                     deriving (Monad, Functor, Applicative, MonadState st)

runUpdate :: Update st a -> st -> a
runUpdate u = evalState (unUpdate u)

-- | Context monad for Query events.
newtype Query st a  = Query { unQuery :: Reader st a }
                     deriving (Monad, Functor, Applicative, MonadReader st)

runQuery :: Query st a -> st -> a
runQuery q = runReader (unQuery q)

-- | Run a query in the Update Monad.
liftQuery :: Query st a -> Update st a
liftQuery q = do
  st <- get
  return (runReader (unQuery q) st)

class Backend b where
  type Key     b
  type Value   b
  query     :: Key b -> Query b (IO (Maybe (Value b)))
  update    :: Key b -> Value b -> Update b (IO ())
  del       :: Key b -> Update b (IO ())

class (Backend b, Serializable (K d) (Key b), Serializable (V d) (Value b)) => Database d b | d -> b where
  type K d
  type V d

  toBackend :: d -> b
  toDatabase :: b -> d

  lUpdate :: Update b a -> Update d a
  lUpdate (Update st) = Update $ state f
    where
      f database = (val, toDatabase backend)
        where
          (val, backend) = runState st $ toBackend database

  lQuery :: Query b a -> Query d a
  lQuery (Query r) = Query $ withReader toBackend r

  find   :: K d ->  Query d (IO (Maybe (V d)))
  find key = lQuery $ fmap (fmap (fromMaybeEither . fmap decode)) (query (encode key))
    where
      fromMaybeEither :: Maybe (Either String a) -> Maybe a
      fromMaybeEither Nothing            = Nothing
      fromMaybeEither (Just (Right a))   = Just a
      fromMaybeEither (Just (Left err))  = error $ "Could not decode: \n" ++ err

  insert :: K d -> V d -> Update d (IO ())
  insert key value = lUpdate $ update (encode key) (encode value)

  delete :: K d -> Update d (IO ())
  delete key = lUpdate $ del $ encode key
