# haskell-rocksdb-test
Test and benchmark of PostgreSQL vs RocksDB with and without transactions.
See schema.sql for the schema of the testdb used by PostgreSQL.

# To run
Set up a database testdb with a table as per schema.sql with a user "testdb" and password
"testdb".
After cloning this repo, from within the project directory run:
```
git submodule init
git submodule update
stack run
```

# Benchmark results
With an AMD FX-6300 CPU and librocksdb v6.17.3
We can see that RocksDB is actually slower than PostgreSQL when running in sync mode.
In async it is much faster but that is not a fair comparison to PostgreSQL running in sync.

PostgreSQL:
```
benchmark "get (keys dont exist)":
 (run 100.0x) OP/S: 5.1k, total time: 0.020s, 197.8µs per OP
benchmark "getForUpdate (keys dont exist)":
 (run 100.0x) OP/S: 4.6k, total time: 0.022s, 217.7µs per OP
benchmark "put (keys dont exist)":
 (run 100.0x) OP/S: 853.9, total time: 0.117s, 1.2ms per OP
benchmark "put (keys exist)":
 (run 100.0x) OP/S: 807.2, total time: 0.124s, 1.2ms per OP
benchmark "get (keys exist)":
 (run 100.0x) OP/S: 4.7k, total time: 0.021s, 214.0µs per OP
benchmark "multiGet 1 (keys exist)":
 (run 100.0x) OP/S: 4.4k, total time: 0.023s, 229.7µs per OP
benchmark "multiGet 10 (keys exist)":
 (run 100.0x) OP/S: 2.3k, total time: 0.044s, 436.8µs per OP
benchmark "multiGet 1000 (keys exist)":
 (run 10.0x) OP/S: 69.7, total time: 0.143s, 14.3ms per OP
benchmark "get 1 in a txn (keys exist)":
 (run 100.0x) OP/S: 2.4k, total time: 0.041s, 414.0µs per OP
benchmark "get 10 in a txn (keys exist)":
 (run 100.0x) OP/S: 423.5, total time: 0.236s, 2.4ms per OP
benchmark "getForUpdate (keys exist)":
 (run 100.0x) OP/S: 862.8, total time: 0.116s, 1.2ms per OP
benchmark "get and put a key in a transaction":
 (run 100.0x) OP/S: 608.6, total time: 0.164s, 1.6ms per OP

RocksDB (no transactions, async):
benchmark "get":
 (run 10000.0x) OP/S: 157.4k, total time: 0.064s, 6.4µs per OP
benchmark "put":
 (run 10000.0x) OP/S: 148.0k, total time: 0.068s, 6.8µs per OP
benchmark "get":
 (run 10000.0x) OP/S: 237.2k, total time: 0.042s, 4.2µs per OP

TxnRocksDB (async):
benchmark "txnGet (cache empty)":
 (run 100.0x) OP/S: 29.4k, total time: 0.003s, 34.0µs per OP
benchmark "txnGet":
 (run 100.0x) OP/S: 31.3k, total time: 0.003s, 31.9µs per OP
---
benchmark "txnGetForUpdate (cache empty)":
 (run 100.0x) OP/S: 28.5k, total time: 0.004s, 35.1µs per OP
benchmark "txnGetForUpdate":
 (run 100.0x) OP/S: 37.2k, total time: 0.003s, 26.9µs per OP
benchmark "txnPut":
 (run 100.0x) OP/S: 42.9k, total time: 0.002s, 23.3µs per OP
benchmark "txnPut x10":
 (run 100.0x) OP/S: 13.7k, total time: 0.007s, 73.0µs per OP
benchmark "txnPut x100":
 (run 100.0x) OP/S: 1.9k, total time: 0.053s, 527.6µs per OP
benchmark "txnGetForUpdate":
 (run 100.0x) OP/S: 31.4k, total time: 0.003s, 31.8µs per OP
benchmark "get and put a key in a transaction":
 (run 100.0x) OP/S: 47.5k, total time: 0.002s, 21.1µs per OP
benchmark "txnGetForUpdate 10 keys":
 (run 100.0x) OP/S: 5.2k, total time: 0.019s, 191.3µs per OP
benchmark "get and put 10 keys in a transaction":
 (run 100.0x) OP/S: 10.0k, total time: 0.010s, 99.6µs per OP

TxnRocksDB (sync):
benchmark "txnGet (cache empty)":
 (run 100.0x) OP/S: 565.7, total time: 0.177s, 1.8ms per OP
benchmark "txnGet":
 (run 100.0x) OP/S: 574.1, total time: 0.174s, 1.7ms per OP
---
benchmark "txnGetForUpdate (cache empty)":
 (run 100.0x) OP/S: 576.5, total time: 0.173s, 1.7ms per OP
benchmark "txnGetForUpdate":
 (run 100.0x) OP/S: 555.9, total time: 0.180s, 1.8ms per OP
benchmark "txnPut":
 (run 100.0x) OP/S: 441.4, total time: 0.227s, 2.3ms per OP
benchmark "txnPut x10":
 (run 100.0x) OP/S: 537.5, total time: 0.186s, 1.9ms per OP
benchmark "txnPut x100":
 (run 100.0x) OP/S: 374.2, total time: 0.267s, 2.7ms per OP
benchmark "txnGetForUpdate":
 (run 100.0x) OP/S: 581.2, total time: 0.172s, 1.7ms per OP
benchmark "get and put a key in a transaction":
 (run 100.0x) OP/S: 553.2, total time: 0.181s, 1.8ms per OP
benchmark "txnGetForUpdate 10 keys":
 (run 100.0x) OP/S: 56.9, total time: 1.756s, 17.6ms per OP
benchmark "get and put 10 keys in a transaction":
 (run 100.0x) OP/S: 510.5, total time: 0.196s, 2.0ms per OP
 ```
