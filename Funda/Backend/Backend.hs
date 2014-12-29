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
newtype Update st a = Update { unUpdate :: StateT st IO a }
                     deriving (Monad, MonadIO, Functor, Applicative, MonadState st)


-- instance Monad (Update st) where
--   return a = Update $ state (\s -> (a,s))
--   (Update st) >>= f = Update $ state (\s -> let (a, s') = runState st s
--                                             in runState (unUpdate . f $ a) s'
--                                      )
--   (Update st) >> (Update st') =  Update $ state (\s -> let (_, s') = runState st s
--                                                        in runState st' s'
--                                      )

runUpdate :: Update st a -> st -> IO a
runUpdate u = evalStateT (unUpdate u)

-- | Context monad for Query events.
newtype Query st a  = Query { unQuery :: ReaderT st IO a }
                     deriving (Monad, Functor, Applicative, MonadReader st)

runQuery :: Query st a -> st -> IO a
runQuery q = runReaderT (unQuery q)

-- | Run a query in the Update Monad.
liftQuery :: Query st a -> Update st a
liftQuery q = do
  st <- get
  liftIO (runReaderT (unQuery q) st)

class Backend b where
  type Key     b
  type Value   b
  query     :: Key b -> Query b (Maybe (Value b))
  update    :: Key b -> Value b -> Update b ()
  del       :: Key b -> Update b ()

class (Backend b,
       Serializable (K d) (Key b),
       Serializable (V d) (Value b)
      ) => Database d b | d -> b where
  type K d
  type V d

  toBackend :: d -> b
  toDatabase :: b -> d

  liftU :: Update b a -> Update d a
  liftU (Update st) = Update $ StateT f
    where
      f database = do
        (val, backend) <- runStateT st $ toBackend database
        return (val, toDatabase backend)

  liftQ :: Query b a -> Query d a
  liftQ (Query r) = Query $ withReaderT toBackend r

  find   :: K d ->  Query d (Maybe (V d))
  find key = liftQ $ fmap (fromMaybeEither . fmap decode) (query (encode key))
    where
      fromMaybeEither :: Maybe (Either String a) -> Maybe a
      fromMaybeEither Nothing            = Nothing
      fromMaybeEither (Just (Right a))   = Just a
      fromMaybeEither (Just (Left err))  = error $ "Could not decode: \n" ++ err

  insert :: K d -> V d -> Update d ()
  insert key value = liftU $ update (encode key) (encode value)

  delete :: K d -> Update d ()
  delete key = liftU $ del $ encode key
