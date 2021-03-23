{-# LANGUAGE FlexibleContexts #-}

module PGKV
    ( get
    , getForUpdate
    , multiGet
    , put
    , deleteAll
    , openDB
    , withDB
    , begin
    , commit
    , Config (..)
    ) where

import Database.PostgreSQL.Simple (Connection, connectHost, connectPort, In(..),
    connectUser, connectPassword, connectDatabase, close, connect,
    defaultConnectInfo, Only(..), query, execute, Query, FromRow, ToRow)
import Data.Pool (createPool, withResource, Pool)
import Data.Word (Word16)
import Control.Monad (void)
import qualified Data.Map.Strict as Map

data DB = DB { pool :: Pool Connection }

data Config = Config {
      db :: String
    , pw :: String
    , user :: String
    , port :: Word16
    , host :: String
    , poolSize :: Int
    }

openDB :: Config -> IO (DB)
openDB cfg = do
    let connectInfo = defaultConnectInfo {
        connectDatabase = db cfg
      , connectPassword = pw cfg
      , connectUser = user cfg
      , connectPort = port cfg
      , connectHost = host cfg
    }
    p <- createPool (connect connectInfo) close 1 24 (poolSize cfg) :: IO (Pool Connection)
    return $ DB { pool = p }

withDB :: DB -> (Connection -> IO m) -> IO m
withDB db f = withResource (pool db) $ \conn -> f conn

getWithQuery :: Connection -> Query -> String -> IO (Maybe String)
getWithQuery conn q k = do
    rows <- query conn q (Only k) :: IO [Only String]
    case rows of
        [(Only v)] -> return $ Just v
        _ -> return Nothing

get :: Connection -> String -> IO (Maybe String)
get conn k = do
    res <- multiGet conn [k]
    case res of
        [mStr] -> return mStr
        [] -> return Nothing

multiGet :: Connection -> [String] -> IO ([Maybe String])
multiGet conn keys = do
    res <- query conn "select k, v from kv where k in ?" $ Only $ In keys :: IO [(String, String)]
    let m = Map.fromList res
    return $ map (\key -> Map.lookup key m) keys

getForUpdate :: Connection -> String -> IO (Maybe String)
getForUpdate conn k = getWithQuery conn "select v from kv where k = ? for update" k

-- can throw SqlError
put :: Connection -> String -> String -> IO ()
put conn k v = void $ execute conn "insert into kv (k,v) values (?,?) \
                             \ on conflict (k) do update set v = excluded.v" (k, v)

begin :: Connection -> IO ()
begin conn = void $ execute conn "begin" ()

commit :: Connection -> IO ()
commit conn = void $ execute conn "commit" ()

deleteAll :: Connection -> IO ()
deleteAll conn = void $ execute conn "delete from kv" ()
