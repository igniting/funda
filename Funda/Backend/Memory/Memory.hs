{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

module Funda.Backend.Memory.Memory where

import           Control.Monad.Reader  as Reader
import qualified Control.Monad.State   as State
import           Data.Acid             (makeAcidic)
import           Data.Acid.Memory.Pure
import qualified Data.ByteString       as B
import qualified Data.Map              as Map
import           Data.SafeCopy
import           Data.Typeable

import           Funda.Backend.Backend hiding (delete, query, update)

type K = B.ByteString
type V = B.ByteString

data MemoryMap = MemoryMap !(Map.Map K V)
                      deriving (Show, Ord, Eq, Typeable)

$(deriveSafeCopy 0 'base ''MemoryMap)

insert :: K -> V -> Update MemoryMap ()
insert key value = do
  MemoryMap m <- State.get
  State.put (MemoryMap (Map.insert key value m))

lookup :: K -> Query MemoryMap (Maybe V)
lookup key = do
  MemoryMap m <- ask
  return (Map.lookup key m)

delete :: K -> Update MemoryMap ()
delete key = do
  MemoryMap m <- State.get
  State.put (MemoryMap (Map.delete key m))

$(makeAcidic ''MemoryMap ['insert, 'lookup, 'delete])

data MemDB = MemDB { db :: AcidState MemoryMap }

instance Monad (QueryM MemDB) where
  return  = MemDBQuery . return
  (MemDBQuery r) >>= f = MemDBQuery (r >>= unQuery . f)

instance Monad (UpdateM MemDB) where
  return = MemDBUpdate . return
  (MemDBUpdate r) >>= f = MemDBUpdate (r >>= unUpdate . f)

-- | Instance declaration for Backend
instance RawBackend MemDB where
  data Key     MemDB = MemDBKey K
  data Value   MemDB = MemDBValue V
  data QueryM  MemDB a = MemDBQuery { unQuery ::  Reader.Reader MemDB a }
  data UpdateM MemDB a = MemDBUpdate { unUpdate :: State.State MemDB a }
  getRaw (MemDBKey key) =  MemDBQuery $ do
    MemDB{..} <- ask
    return $ MemDBValue `fmap` (query db (Lookup $ key))
  putRaw (MemDBKey key) (MemDBValue val) = MemDBUpdate $ do
    MemDB{..} <- State.get
    _ <- State.put $ MemDB $ update_ db (Insert key val)
    return ()
  deleteRaw (MemDBKey key) = MemDBUpdate $ do
    MemDB{..} <- State.get
    _ <- State.put $ MemDB $ update_ db (Delete key)
    return ()
  runQuery  (MemDBQuery r) = runReader r
  runUpdate (MemDBUpdate r) = State.evalState r >> return ()

openMemDB :: MemDB
openMemDB = MemDB $ openAcidState (MemoryMap  Map.empty)
