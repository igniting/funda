{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Funda.Backend.Bitcask.Bitcask where

import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Binary.Get
import           Data.Binary.Put
import qualified Data.ByteString.Char8         as C
import qualified Data.ByteString.Lazy          as BL
import qualified Data.HashTable.IO             as HT
import           Funda.Backend.Backend
import           Funda.Backend.Bitcask.Parsing
import           Funda.Backend.Bitcask.Types   as Types
import           System.Directory
import           System.IO

type RawBitcask = Bitcask Types.K Types.V

instance Backend RawBitcask where
  type Key     RawBitcask = Types.K
  type Value   RawBitcask = Types.V
  query k = do
    db <- ask
    liftIO $ query' db where
      query' db = do
        maybeOffset <- HT.lookup (offsetTable db) k
        case maybeOffset of
             Nothing -> return Nothing
             (Just offset) -> if offset == 0
                                 then return Nothing
                                 else
                                 do
                                   ht <- openBinaryFile (currRecords db) ReadMode
                                   hSeek ht AbsoluteSeek (offset - 1)
                                   l <- BL.hGetContents ht
                                   return (Just (dValue (runGet parseDataLog l)))

  update k v = do
    db <- get
    liftIO $ update' db where
      update' db = do
        ht <- openBinaryFile (currRecords db) AppendMode
        offset <- hFileSize ht
        BL.hPutStr ht (runPut (deserializeData k v))
        hClose ht
        htH <- openBinaryFile (currHint db) AppendMode
        BL.hPutStr htH (runPut (deserializeHint k (offset + 1)))
        hClose htH
        HT.insert (offsetTable db) k (offset + 1)
        return ()

  del k = do
    db <- get
    liftIO $ del' db k where
      del' db k = do
        HT.delete (offsetTable db) k
        ht <- openBinaryFile (currHint db) AppendMode
        BL.hPutStr ht (runPut (deserializeHint k 0))
        hClose ht
        return ()

initDB :: Config -> IO RawBitcask
initDB cfg = do
  -- Create the records file if not exists
  check <- doesFileExist (recordsFileName cfg)
  if check
     then
     do
       allcontent <- BL.readFile (hintFileName cfg)
       m <- HT.fromList (map getKeyOffsetPair (runGet parseHintLogs allcontent))
       return (Bitcask (recordsFileName cfg) (hintFileName cfg) m)
       else
       do
         writeFile (recordsFileName cfg) ""
         writeFile (hintFileName cfg) ""
         m <- HT.new
         return (Bitcask (recordsFileName cfg) (hintFileName cfg) m)

testDB :: IO ()
testDB = do
  db <- initDB (Config "/tmp/records.dat" "/tmp/hints.dat")
  runUpdate ops db where
    ops :: Update RawBitcask ()
    ops = do
      _ <- update (C.pack "foo") (C.pack "FOO")
      _ <- update (C.pack "bar") (C.pack "BAR")
      foo <- liftQuery $ query (C.pack "foo")
      bar <- liftQuery $ query (C.pack "bar")
      liftIO $ print foo
      liftIO $ print bar
      _ <- del (C.pack "foo")
      foo' <- liftQuery $ query (C.pack "foo")
      liftIO $ print foo'
