
module Lib
    ( run
    ) where

import Data.Default (def)
import Database.RocksDB (Config(..), withDB, get, put, withTxn, withTxnDB, txnGet, txnPut,
                        txnGetForUpdate)
import Data.ByteString.Internal as BS (ByteString)
import Data.ByteString.Char8 as BS (pack)
import Bench (bench, benchTime)
import Data.Maybe (fromJust)
import Control.Monad (forM_)
import Control.Exception (assert)
import qualified PGKV as P (get, begin, commit, getForUpdate, put, deleteAll, Config(..),
                            openDB, withDB, multiGet)

conf :: Config
conf = def { createIfMissing = True
           , errorIfExists   = False
           , bloomFilter     = True
           , prefixLength    = Just 3
           , sync            = True
           }

shouldReturn :: Eq a => IO a -> a -> IO ()
shouldReturn action shouldVal = action >>= (\res -> assert (res == shouldVal) (return ()))

run :: IO ()
run = do
    -- psql
    putStrLn "\nPostgreSQL:"
    let pCfg = P.Config {
        P.db = "testdb"
      , P.pw = "testdb"
      , P.user = "testdb"
      , P.port = 5432
      , P.host = "127.0.0.1"
      , P.poolSize = 80
    }
    db <- P.openDB pCfg

    let nIter = 100
    let rocksNIter = 100

    P.withDB db $ \conn -> do
        -- clear kv table
        P.deleteAll conn

        bench nIter "get (keys dont exist)" $ \i -> P.get conn ("k" ++ (show i))
        bench nIter "getForUpdate (keys dont exist)" $ \i -> P.getForUpdate conn ("k" ++ (show i))
        bench nIter "put (keys dont exist)" $ \i -> P.put conn ("k" ++ (show i)) ("v" ++ (show i))
        bench nIter "put (keys exist)" $ \i -> P.put conn ("k" ++ (show i)) ("v" ++ (show i))

        P.get conn "k1" `shouldReturn` Just "v1"
        P.multiGet conn ["k1"] `shouldReturn` [Just "v1"]
        P.multiGet conn ["k1","k2"] `shouldReturn` [Just "v1", Just "v2"]
        P.multiGet conn ["k2","k1"] `shouldReturn` [Just "v2", Just "v1"]
        P.multiGet conn ["kinvalid","k1","kinvalid2"] `shouldReturn`
            [Nothing, Just "v1", Nothing]

        bench nIter "get (keys exist)" $ \i -> P.get conn ("k" ++ (show i))
        bench nIter "multiGet 1 (keys exist)" $ \i -> P.multiGet conn [("k" ++ (show i))]
        bench nIter "multiGet 10 (keys exist)" $ \_ -> do
            P.multiGet conn (map (\i -> ("k" ++ (show i))) [1..10])
        bench 10 "multiGet 1000 (keys exist)" $ \_ -> do
            P.multiGet conn (map (\i -> ("k" ++ (show i))) [1..1000])
        bench nIter "get 1 in a txn (keys exist)" $ \i -> do
            P.begin conn
            P.get conn ("k" ++ (show i))
            P.commit conn
        bench nIter "get 10 in a txn (keys exist)" $ \_ -> do
            P.begin conn
            mapM_ (\i -> P.get conn ("k" ++ (show i)) ) [1..10]
            P.commit conn
        bench nIter "getForUpdate (keys exist)" $ \i -> P.getForUpdate conn ("k" ++ (show i))
        bench nIter "get and put a key in a transaction" $ \i -> do
            P.begin conn
            let key = ("k" ++ (show i))
            res <- P.getForUpdate conn key
            let val = fromJust res
            P.put conn key (val ++ "'")
            P.commit conn

    -- rocksdb
    withDB "db" (conf{ sync = False }) $ \db -> do
        putStrLn "\nRocksDB (no transactions, async):"
        bench 10000 "get" $ \i -> get db (BS.pack $ show i)
        bench 10000 "put" $ \i -> put db (BS.pack $ show i) "v"
        bench 10000 "get" $ \i -> get db (BS.pack $ show i)

    -- txn rocksdb async
    putStrLn "\nTxnRocksDB (async):"
    withTxnDB "txndb" conf{ sync = False } $ \db -> do
        bench rocksNIter "txnGet (cache empty)" $ \i -> do
            withTxn db $ \txn -> txnGet txn db (BS.pack $ show i)
        bench rocksNIter "txnGet" $ \i -> do
            withTxn db $ \txn -> txnGet txn db (BS.pack $ show i)

    putStrLn "---"
    withTxnDB "txndb2" conf{ sync = False } $ \db -> do
        bench rocksNIter "txnGetForUpdate (cache empty)" $ \i -> do
            withTxn db $ \txn -> txnGetForUpdate txn db (BS.pack $ show i)
        bench rocksNIter "txnGetForUpdate" $ \i -> do
            withTxn db $ \txn -> txnGetForUpdate txn db (BS.pack $ show i)
        bench rocksNIter "txnPut" $ \i -> do
            withTxn db $ \txn -> txnPut txn (BS.pack $ show i) "v"
        bench rocksNIter "txnPut x10" $ \_ ->
            withTxn db $ \txn ->
                forM_ [1..10] $ \i ->
                    txnPut txn (BS.pack $ show i) "v"
        bench rocksNIter "txnPut x100" $ \_ ->
            withTxn db $ \txn ->
                forM_ [1..100] $ \i ->
                    txnPut txn (BS.pack $ show i) "v"
        bench rocksNIter "txnGetForUpdate" $ \i -> do
            withTxn db $ \txn -> txnGetForUpdate txn db (BS.pack $ show i)
        bench rocksNIter "get and put a key in a transaction" $ \i -> do
            withTxn db $ \txn -> do
                txnGetForUpdate txn db (BS.pack $ show i)
                txnPut txn (BS.pack $ show i) "v"
        bench rocksNIter "txnGetForUpdate 10 keys" $ \_ -> do
            forM_ [1..10] $ \i ->
                withTxn db $ \txn -> txnGetForUpdate txn db (BS.pack $ show i)
        bench rocksNIter "get and put 10 keys in a transaction" $ \_ -> do
            withTxn db $ \txn -> do
                forM_ [1..10] $ \i ->
                    txnGetForUpdate txn db (BS.pack $ show i)
                forM_ [1..10] $ \i ->
                    txnPut txn (BS.pack $ show i) "v"

    -- txn rocksdb
    putStrLn "\nTxnRocksDB (sync):"
    withTxnDB "txndb" conf $ \db -> do
        bench rocksNIter "txnGet (cache empty)" $ \i -> do
            withTxn db $ \txn -> txnGet txn db (BS.pack $ show i)
        bench rocksNIter "txnGet" $ \i -> do
            withTxn db $ \txn -> txnGet txn db (BS.pack $ show i)

    putStrLn "---"
    withTxnDB "txndb2" conf $ \db -> do
        bench rocksNIter "txnGetForUpdate (cache empty)" $ \i -> do
            withTxn db $ \txn -> txnGetForUpdate txn db (BS.pack $ show i)
        bench rocksNIter "txnGetForUpdate" $ \i -> do
            withTxn db $ \txn -> txnGetForUpdate txn db (BS.pack $ show i)
        bench rocksNIter "txnPut" $ \i -> do
            withTxn db $ \txn -> txnPut txn (BS.pack $ show i) "v"
        bench rocksNIter "txnPut x10" $ \_ ->
            withTxn db $ \txn ->
                forM_ [1..10] $ \i ->
                    txnPut txn (BS.pack $ show i) "v"
        bench rocksNIter "txnPut x100" $ \_ ->
            withTxn db $ \txn ->
                forM_ [1..100] $ \i ->
                    txnPut txn (BS.pack $ show i) "v"
        bench rocksNIter "txnGetForUpdate" $ \i -> do
            withTxn db $ \txn -> txnGetForUpdate txn db (BS.pack $ show i)
        bench rocksNIter "get and put a key in a transaction" $ \i -> do
            withTxn db $ \txn -> do
                txnGetForUpdate txn db (BS.pack $ show i)
                txnPut txn (BS.pack $ show i) "v"
        bench rocksNIter "txnGetForUpdate 10 keys" $ \_ -> do
            forM_ [1..10] $ \i ->
                withTxn db $ \txn -> txnGetForUpdate txn db (BS.pack $ show i)
        bench rocksNIter "get and put 10 keys in a transaction" $ \_ -> do
            withTxn db $ \txn -> do
                forM_ [1..10] $ \i ->
                    txnGetForUpdate txn db (BS.pack $ show i)
                forM_ [1..10] $ \i ->
                    txnPut txn (BS.pack $ show i) "v"
