all:
	stack build --ghc-options="-fhide-source-paths -freverse-errors -O3 -Wall -Werror -Wcompat -Wunused-matches -Wunused-foralls  -Wincomplete-patterns -Wunused-imports -Wdodgy-exports -Wdodgy-imports -Wincomplete-uni-patterns -Wincomplete-record-updates -Wmissing-local-signatures -Wmissing-export-lists -Wpartial-fields -Wmissing-deriving-strategies -Wunused-packages -Wmissing-exported-signatures -Widentities -Wredundant-constraints -threaded -rtsopts -with-rtsopts=-N"

dev:
	stack build --ghc-options="-O3 -threaded -rtsopts -with-rtsopts=-N"

run_dev: dev
	stack exec haskell-rocksdb-test

run: all
	stack exec haskell-rocksdb-test

halive:
	stack exec halive app/Main.hs src

ghci: all
	stack ghci haskell-rocksdb-test

run_single_core: all
	stack exec haskell-rocksdb-test --rts-options=-N1

curl_test:
	curl http://127.0.0.1:9000 && echo

bench:
	echo "1 connection:"
	wrk -c 1 -d 1 -t 1 http://127.0.0.1:9000
	echo "200 connections:"
	wrk -c 200 -d 3 -t 1 http://127.0.0.1:9000

bench_long:
	wrk -c 200 -d 180 -t 1 http://127.0.0.1:9000
